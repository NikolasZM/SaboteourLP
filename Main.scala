import scala.util.Random

// ─────────────────────────────────────────────
//  ENUMERACIONES
// ─────────────────────────────────────────────

enum Rol:
  case BUSCADOR, SABOTEADOR

enum Herramienta:
  case PICO, CARRETILLA, FAROL

enum TipoAccion:
  case SABOTAJE(herramienta: Herramienta)
  case REPARACION(herramientas: List[Herramienta])
  case MAPA
  case DERRUMBE

// ─────────────────────────────────────────────
//  RESULTADO DE ACCIÓN (en lugar de excepciones)
// ─────────────────────────────────────────────

enum ResultadoAccion:
  case Exito(nuevoJuego: Juego, mensaje: String = "")
  case Error(razon: String)

// ─────────────────────────────────────────────
//  ESTADO DE LA PARTIDA
// ─────────────────────────────────────────────

enum EstadoPartida:
  case EnCurso
  case GanoBuscadores
  case GanoSaboteadores

// ─────────────────────────────────────────────
//  CARTAS
// ─────────────────────────────────────────────

sealed trait Carta:
  def id: Int
  def nombre: String

case class CartaTunel(
    id: Int,
    nombre: String,
    arriba: Boolean,
    abajo: Boolean,
    izquierda: Boolean,
    derecha: Boolean,
    esCallejonSinSalida: Boolean,
    esMeta: Boolean = false,
    estaOculta: Boolean = false,
    esOro: Boolean = false
) extends Carta

case class CartaAccion(
    id: Int,
    nombre: String,
    tipoEfecto: TipoAccion
) extends Carta

// ─────────────────────────────────────────────
//  JUGADOR
// ─────────────────────────────────────────────

case class Jugador(
    id: Int,
    nombre: String,
    rol: Rol,
    mano: List[Carta],
    herramientasRotas: List[Herramienta] = Nil
):
  def estaBloqueado: Boolean = herramientasRotas.nonEmpty

  def eliminarCartaDeMano(cartaId: Int): Jugador =
    this.copy(mano = this.mano.filterNot(_.id == cartaId))

  def romperHerramienta(h: Herramienta): Jugador =
    if herramientasRotas.contains(h) then this
    else this.copy(herramientasRotas = h :: herramientasRotas)

  // Repara la primera herramienta de la lista que esté efectivamente rota
  def repararHerramienta(hs: List[Herramienta]): Option[Jugador] =
    hs.find(herramientasRotas.contains) match
      case Some(h) => Some(this.copy(herramientasRotas = herramientasRotas.filterNot(_ == h)))
      case None    => None

// ─────────────────────────────────────────────
//  POSICIÓN
// ─────────────────────────────────────────────

case class Posicion(x: Int, y: Int):
  def arriba: Posicion    = Posicion(x, y - 1)
  def abajo: Posicion     = Posicion(x, y + 1)
  def izquierda: Posicion = Posicion(x - 1, y)
  def derecha: Posicion   = Posicion(x + 1, y)

// ─────────────────────────────────────────────
//  TABLERO
// ─────────────────────────────────────────────

case class Tablero(
    cuadricula: Map[Posicion, CartaTunel],
    posicionInicio: Posicion,
    posicionesMeta: List[Posicion]
):
  // ── Validación corregida (AND, no OR) ─────────────────────────────────────
  // Todos los vecinos existentes deben ser compatibles con la carta nueva.
  def validarColocacion(pos: Posicion, carta: CartaTunel): Boolean =
    if cuadricula.contains(pos) then return false

    val vecinoArr = cuadricula.get(pos.arriba)
    val vecinoAba = cuadricula.get(pos.abajo)
    val vecinoIzq = cuadricula.get(pos.izquierda)
    val vecinoDer = cuadricula.get(pos.derecha)

    val tieneVecino = vecinoArr.isDefined || vecinoAba.isDefined ||
                      vecinoIzq.isDefined || vecinoDer.isDefined
    if !tieneVecino then return false

    // forall es true cuando no hay vecino => solo falla si hay vecino incompatible
    val validoArr = vecinoArr.forall(v => v.estaOculta || v.abajo      == carta.arriba)
    val validoAba = vecinoAba.forall(v => v.estaOculta || v.arriba     == carta.abajo)
    val validoIzq = vecinoIzq.forall(v => v.estaOculta || v.derecha    == carta.izquierda)
    val validoDer = vecinoDer.forall(v => v.estaOculta || v.izquierda  == carta.derecha)

    validoArr && validoAba && validoIzq && validoDer

  // ── Eliminar carta (Derrumbe) ─────────────────────────────────────────────
  def eliminarCarta(pos: Posicion): Option[Tablero] =
    if pos == posicionInicio || posicionesMeta.contains(pos) then None
    else cuadricula.get(pos).map(_ => this.copy(cuadricula = cuadricula - pos))

  // ── Comprobar ruta al oro (BFS) ───────────────────────────────────────────
  def hayRutaAlOro: Boolean =
    posicionesMeta.exists { metaPos =>
      cuadricula.get(metaPos).exists(m => m.esOro && !m.estaOculta && hayConexion(posicionInicio, metaPos))
    }

  def hayConexion(desde: Posicion, hasta: Posicion): Boolean =
    def bfs(cola: List[Posicion], visitados: Set[Posicion]): Boolean =
      cola match
        case Nil => false
        case actual :: resto =>
          if actual == hasta then true
          else if visitados.contains(actual) then bfs(resto, visitados)
          else
            val vecinos = cuadricula.get(actual)
              .map(c => vecinosConectados(actual, c))
              .getOrElse(Nil)
              .filterNot(visitados.contains)
            bfs(resto ++ vecinos, visitados + actual)
    bfs(List(desde), Set.empty)

  private def vecinosConectados(pos: Posicion, carta: CartaTunel): List[Posicion] =
    List(
      Option.when(carta.arriba)(pos.arriba),
      Option.when(carta.abajo)(pos.abajo),
      Option.when(carta.izquierda)(pos.izquierda),
      Option.when(carta.derecha)(pos.derecha)
    ).flatten.filter { vecPos =>
      cuadricula.get(vecPos).exists { vecCarta =>
        if vecPos == pos.arriba         then vecCarta.abajo
        else if vecPos == pos.abajo     then vecCarta.arriba
        else if vecPos == pos.izquierda then vecCarta.derecha
        else                                 vecCarta.izquierda
      }
    }

// ─────────────────────────────────────────────
//  MAZO
// ─────────────────────────────────────────────

case class Mazo(cartasRobo: List[Carta], cartasDescarte: List[Carta]):
  def robarCarta: (Option[Carta], Mazo) =
    cartasRobo match
      case carta :: resto => (Some(carta), this.copy(cartasRobo = resto))
      case Nil            => (None, this)

  def descartar(carta: Carta): Mazo =
    this.copy(cartasDescarte = carta :: cartasDescarte)

// ─────────────────────────────────────────────
//  JUEGO
// ─────────────────────────────────────────────

case class Juego(
    listaJugadores: List[Jugador],
    tablero: Tablero,
    mazo: Mazo,
    turnoActual: Int,
    indiceJugadorActual: Int = 0,
    estadoPartida: EstadoPartida = EstadoPartida.EnCurso,
    mensajeAlerta: String = ""
):
  def jugadorActual: Jugador = listaJugadores(indiceJugadorActual)

  private def siguienteIndice: Int = (indiceJugadorActual + 1) % listaJugadores.size

  private def actualizarJugador(j: Jugador): List[Jugador] =
    listaJugadores.map(jj => if jj.id == j.id then j else jj)

  // ── Inicializar: repartir cartas ──────────────────────────────────────────
  def inicializarPartida(): Juego =
    val cartasPorJugador = listaJugadores.size match
      case n if n <= 5 => 6
      case n if n <= 7 => 5
      case _           => 4

    val (jugadoresConCartas, mazoFinal) =
      listaJugadores.foldLeft((List.empty[Jugador], mazo)) { case ((jsAcc, mazoAcc), jugador) =>
        val (mano, restoMazo) = mazoAcc.cartasRobo.splitAt(cartasPorJugador)
        (jsAcc :+ jugador.copy(mano = mano), mazoAcc.copy(cartasRobo = restoMazo))
      }

    this.copy(listaJugadores = jugadoresConCartas, mazo = mazoFinal)

  // ── Colocar carta de túnel ────────────────────────────────────────────────
  def colocarTunel(cartaId: Int, pos: Posicion): ResultadoAccion =
    val jugador = jugadorActual

    if jugador.estaBloqueado then
      return ResultadoAccion.Error(s"${jugador.nombre} tiene herramientas rotas y no puede colocar túneles.")

    jugador.mano.collectFirst { case c: CartaTunel if c.id == cartaId => c } match
      case None => ResultadoAccion.Error("Esa carta de túnel no está en tu mano.")
      case Some(carta) =>
        if !tablero.validarColocacion(pos, carta) then
          ResultadoAccion.Error("Posición inválida: la carta no conecta correctamente.")
        else
          val nuevoTablero = tablero.copy(cuadricula = tablero.cuadricula + (pos -> carta))

          // Si se alcanzó el oro, revelar la meta ganadora y terminar
          val (tableroFinal, msg, estado) =
            if nuevoTablero.hayRutaAlOro then
              val tableroConOroVisible = nuevoTablero.posicionesMeta.foldLeft(nuevoTablero) { (t, metaPos) =>
                t.cuadricula.get(metaPos).filter(_.esOro) match
                  case Some(meta) => t.copy(cuadricula = t.cuadricula + (metaPos -> meta.copy(estaOculta = false)))
                  case None       => t
              }
              (tableroConOroVisible, s"¡¡${jugador.nombre} y los Buscadores encontraron el ORO!!", EstadoPartida.GanoBuscadores)
            else
              (nuevoTablero, "", EstadoPartida.EnCurso)

          ResultadoAccion.Exito(avanzarTurno(jugador, cartaId, tableroFinal, estado, msg), msg)

  // ── Sabotaje ──────────────────────────────────────────────────────────────
  def aplicarSabotaje(cartaId: Int, jugadorObjetivoId: Int): ResultadoAccion =
    val jugador = jugadorActual
    jugador.mano.collectFirst { case c: CartaAccion if c.id == cartaId => c } match
      case None => ResultadoAccion.Error("Esa carta de acción no está en tu mano.")
      case Some(carta) =>
        carta.tipoEfecto match
          case TipoAccion.SABOTAJE(herramienta) =>
            listaJugadores.find(_.id == jugadorObjetivoId) match
              case None => ResultadoAccion.Error("Jugador objetivo no encontrado.")
              case Some(objetivo) =>
                if objetivo.herramientasRotas.contains(herramienta) then
                  ResultadoAccion.Error(s"${objetivo.nombre} ya tiene roto el $herramienta.")
                else
                  val objetivoActualizado = objetivo.romperHerramienta(herramienta)
                  val nuevosJugadores = listaJugadores.map(j => if j.id == jugadorObjetivoId then objetivoActualizado else j)
                  val msg = s"¡${jugador.nombre} saboteó el $herramienta de ${objetivo.nombre}!"
                  val juegoBase = avanzarTurno(jugador, cartaId, tablero, EstadoPartida.EnCurso, msg)
                  ResultadoAccion.Exito(juegoBase.copy(listaJugadores = nuevosJugadores), msg)
          case _ => ResultadoAccion.Error("Esa carta no es de sabotaje.")

  // ── Reparación ────────────────────────────────────────────────────────────
  def aplicarReparacion(cartaId: Int, jugadorObjetivoId: Int): ResultadoAccion =
    val jugador = jugadorActual
    jugador.mano.collectFirst { case c: CartaAccion if c.id == cartaId => c } match
      case None => ResultadoAccion.Error("Esa carta de acción no está en tu mano.")
      case Some(carta) =>
        carta.tipoEfecto match
          case TipoAccion.REPARACION(herramientas) =>
            listaJugadores.find(_.id == jugadorObjetivoId) match
              case None => ResultadoAccion.Error("Jugador objetivo no encontrado.")
              case Some(objetivo) =>
                objetivo.repararHerramienta(herramientas) match
                  case None => ResultadoAccion.Error(s"${objetivo.nombre} no tiene ninguna de esas herramientas rotas.")
                  case Some(objetivoReparado) =>
                    val nuevosJugadores = listaJugadores.map(j => if j.id == jugadorObjetivoId then objetivoReparado else j)
                    val msg = s"${jugador.nombre} reparó una herramienta de ${objetivo.nombre}."
                    val juegoBase = avanzarTurno(jugador, cartaId, tablero, EstadoPartida.EnCurso, msg)
                    ResultadoAccion.Exito(juegoBase.copy(listaJugadores = nuevosJugadores), msg)
          case _ => ResultadoAccion.Error("Esa carta no es de reparación.")

  // ── Mapa (Lupa) ───────────────────────────────────────────────────────────
  def usarMapa(cartaId: Int, posicionMeta: Posicion): ResultadoAccion =
    val jugador = jugadorActual
    jugador.mano.collectFirst { case c: CartaAccion if c.id == cartaId => c } match
      case None => ResultadoAccion.Error("Esa carta de mapa no está en tu mano.")
      case Some(carta) =>
        carta.tipoEfecto match
          case TipoAccion.MAPA =>
            tablero.cuadricula.get(posicionMeta) match
              case None => ResultadoAccion.Error("No hay carta en esa posición.")
              case Some(cartaMeta) if !cartaMeta.esMeta =>
                ResultadoAccion.Error("La lupa solo se puede usar sobre una carta de meta.")
              case Some(cartaMeta) =>
                val contenido = if cartaMeta.esOro then "¡¡ORO!!" else "Carbón."
                val msg = s"[Solo ${jugador.nombre}] Meta (${posicionMeta.x},${posicionMeta.y}): $contenido"
                ResultadoAccion.Exito(avanzarTurno(jugador, cartaId, tablero, EstadoPartida.EnCurso, msg), msg)
          case _ => ResultadoAccion.Error("Esa carta no es de mapa.")

  // ── Derrumbe ──────────────────────────────────────────────────────────────
  def aplicarDerrumbe(cartaId: Int, posicionObjetivo: Posicion): ResultadoAccion =
    val jugador = jugadorActual
    jugador.mano.collectFirst { case c: CartaAccion if c.id == cartaId => c } match
      case None => ResultadoAccion.Error("Esa carta de derrumbe no está en tu mano.")
      case Some(carta) =>
        carta.tipoEfecto match
          case TipoAccion.DERRUMBE =>
            tablero.eliminarCarta(posicionObjetivo) match
              case None => ResultadoAccion.Error("No se puede eliminar esa carta (inicio, meta o vacía).")
              case Some(nuevoTablero) =>
                val msg = s"${jugador.nombre} derrumbó el túnel en (${posicionObjetivo.x}, ${posicionObjetivo.y})."
                ResultadoAccion.Exito(avanzarTurno(jugador, cartaId, nuevoTablero, EstadoPartida.EnCurso, msg), msg)
          case _ => ResultadoAccion.Error("Esa carta no es de derrumbe.")

  // ── Descartar ─────────────────────────────────────────────────────────────
  def descartarCarta(cartaId: Int): ResultadoAccion =
    val jugador = jugadorActual
    jugador.mano.find(_.id == cartaId) match
      case None => ResultadoAccion.Error("Esa carta no está en tu mano.")
      case Some(carta) =>
        val mazoConDescarte  = mazo.descartar(carta)
        val jugadorSinCarta  = jugador.eliminarCartaDeMano(cartaId)
        val (cartaRobada, mazoFinal) = mazoConDescarte.robarCarta
        val manoFinal        = cartaRobada.map(jugadorSinCarta.mano :+ _).getOrElse(jugadorSinCarta.mano)
        val jugadorFinal     = jugadorSinCarta.copy(mano = manoFinal)
        val nuevosJugadores  = actualizarJugador(jugadorFinal)
        val msg              = s"${jugador.nombre} descartó una carta."

        val nuevoEstado =
          if mazoFinal.cartasRobo.isEmpty && nuevosJugadores.forall(_.mano.isEmpty)
          then EstadoPartida.GanoSaboteadores
          else EstadoPartida.EnCurso

        ResultadoAccion.Exito(
          this.copy(
            listaJugadores      = nuevosJugadores,
            mazo                = mazoFinal,
            turnoActual         = turnoActual + 1,
            indiceJugadorActual = siguienteIndice,
            estadoPartida       = nuevoEstado,
            mensajeAlerta       = msg
          ),
          msg
        )

  // ── Helper: avanzar turno + robar carta ───────────────────────────────────
  private def avanzarTurno(
    jugador: Jugador,
    cartaIdUsada: Int,
    nuevoTablero: Tablero,
    nuevoEstado: EstadoPartida,
    alerta: String
  ): Juego =
    val jugadorSinCarta         = jugador.eliminarCartaDeMano(cartaIdUsada)
    val (cartaRobada, nuevoMazo) = mazo.robarCarta
    val manoFinal               = cartaRobada.map(jugadorSinCarta.mano :+ _).getOrElse(jugadorSinCarta.mano)
    val jugadorFinal            = jugadorSinCarta.copy(mano = manoFinal)
    val nuevosJugadores         = actualizarJugador(jugadorFinal)

    val estadoFinal =
      if nuevoEstado != EstadoPartida.EnCurso then nuevoEstado
      else if nuevoMazo.cartasRobo.isEmpty && nuevosJugadores.forall(_.mano.isEmpty)
      then EstadoPartida.GanoSaboteadores
      else EstadoPartida.EnCurso

    this.copy(
      tablero             = nuevoTablero,
      listaJugadores      = nuevosJugadores,
      mazo                = nuevoMazo,
      turnoActual         = turnoActual + 1,
      indiceJugadorActual = siguienteIndice,
      estadoPartida       = estadoFinal,
      mensajeAlerta       = alerta
    )

// ─────────────────────────────────────────────
//  GENERADOR DE MAZO
// ─────────────────────────────────────────────

object GeneradorMazo:
  def crearMazoMezclado(): Mazo =
    // Usamos un iterador sobre un rango para generar IDs únicos funcionalmente
    val idIter = Iterator.from(1)
    def nextId(): Int = idIter.next()

    val tuneles: List[CartaTunel] =
      List.tabulate(15)(_ => CartaTunel(nextId(), "Túnel Cruz",      arriba=true,  abajo=true,  izquierda=true,  derecha=true,  esCallejonSinSalida=false)) ++
      List.tabulate(10)(_ => CartaTunel(nextId(), "Túnel Recto H",   arriba=false, abajo=false, izquierda=true,  derecha=true,  esCallejonSinSalida=false)) ++
      List.tabulate(10)(_ => CartaTunel(nextId(), "Túnel Recto V",   arriba=true,  abajo=true,  izquierda=false, derecha=false, esCallejonSinSalida=false)) ++
      List.tabulate(6) (_ => CartaTunel(nextId(), "Callejón Cruz X", arriba=true,  abajo=true,  izquierda=true,  derecha=true,  esCallejonSinSalida=true))

    val sabotajes: List[CartaAccion] =
      Herramienta.values.toList.flatMap(h =>
        List.tabulate(3)(_ => CartaAccion(nextId(), s"Sabotaje ($h)", TipoAccion.SABOTAJE(h)))
      )

    val reparacionesSimples: List[CartaAccion] =
      Herramienta.values.toList.flatMap(h =>
        List.tabulate(2)(_ => CartaAccion(nextId(), s"Reparar ($h)", TipoAccion.REPARACION(List(h))))
      )

    val reparacionesDobles: List[CartaAccion] = List(
      CartaAccion(nextId(), "Reparar Pico/Carretilla",  TipoAccion.REPARACION(List(Herramienta.PICO, Herramienta.CARRETILLA))),
      CartaAccion(nextId(), "Reparar Pico/Farol",       TipoAccion.REPARACION(List(Herramienta.PICO, Herramienta.FAROL))),
      CartaAccion(nextId(), "Reparar Carretilla/Farol", TipoAccion.REPARACION(List(Herramienta.CARRETILLA, Herramienta.FAROL)))
    )

    val mapas: List[CartaAccion] =
      List.tabulate(6)(_ => CartaAccion(nextId(), "Lupa (Mapa)", TipoAccion.MAPA))

    val derrumbes: List[CartaAccion] =
      List.tabulate(3)(_ => CartaAccion(nextId(), "Derrumbamiento", TipoAccion.DERRUMBE))

    Mazo(
      cartasRobo    = Random.shuffle(tuneles ++ sabotajes ++ reparacionesSimples ++ reparacionesDobles ++ mapas ++ derrumbes),
      cartasDescarte = Nil
    )

// ─────────────────────────────────────────────
//  FACTORY: construir juego inicial
// ─────────────────────────────────────────────

object FabricaJuego:

  def crearTableroInicial(): Tablero =
    val posInicio = Posicion(2, 3)
    val posMetas  = List(Posicion(10, 1), Posicion(10, 3), Posicion(10, 5))
    val indiceOro = Random.nextInt(3)

    val cartasMeta = posMetas.zipWithIndex.map { case (pos, i) =>
      pos -> CartaTunel(
        id                  = -(i + 1),
        nombre              = "Destino Oculto",
        arriba              = true, abajo = true, izquierda = true, derecha = true,
        esCallejonSinSalida = false,
        esMeta              = true,
        estaOculta          = true,
        esOro               = (i == indiceOro)
      )
    }.toMap

    val cartaInicio = CartaTunel(0, "Inicio", arriba=true, abajo=true, izquierda=true, derecha=true, esCallejonSinSalida=false)
    Tablero(cartasMeta + (posInicio -> cartaInicio), posInicio, posMetas)

  def crearJuego(nombresJugadores: List[String]): Juego =
    val n = nombresJugadores.size
    require(n >= 3 && n <= 10, "El juego requiere entre 3 y 10 jugadores.")

    val numSaboteadores = n match
      case 3 | 4 => 1
      case 5 | 6 => 2
      case 7 | 8 => 3
      case _     => 4

    val roles = Random.shuffle(
      List.fill(numSaboteadores)(Rol.SABOTEADOR) ++
      List.fill(n - numSaboteadores)(Rol.BUSCADOR)
    )

    val jugadores = nombresJugadores.zipWithIndex.map { case (nombre, i) =>
      Jugador(id = i + 1, nombre = nombre, rol = roles(i), mano = Nil)
    }

    Juego(
      listaJugadores      = jugadores,
      tablero             = crearTableroInicial(),
      mazo                = GeneradorMazo.crearMazoMezclado(),
      turnoActual         = 1,
      indiceJugadorActual = 0,
      estadoPartida       = EstadoPartida.EnCurso
    ).inicializarPartida()