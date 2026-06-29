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
    esOro: Boolean = false,
    imagenVolteada: Boolean = false  // solo para la UI: indica si dibujar rotada 180°
) extends Carta:
  def voltear: CartaTunel = this.copy(
    arriba         = abajo,
    abajo          = arriba,
    izquierda      = derecha,
    derecha        = izquierda,
    imagenVolteada = !imagenVolteada
  )

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

object Tablero:
  // Área jugable: 9 columnas x 5 filas (coincide con inicio x=2 y metas x=10, y=1..5)
  val limiteIzquierdo = 2
  val limiteDerecho   = 10
  val limiteArriba    = 1
  val limiteAbajo     = 5

case class Tablero(
    cuadricula: Map[Posicion, CartaTunel],
    posicionInicio: Posicion,
    posicionesMeta: List[Posicion]
):
  // ── ¿La posición está dentro del área jugable de 9x5? ─────────────────────
  def dentroDelTablero(pos: Posicion): Boolean =
    pos.x >= Tablero.limiteIzquierdo && pos.x <= Tablero.limiteDerecho &&
    pos.y >= Tablero.limiteArriba    && pos.y <= Tablero.limiteAbajo

  // ── Validación: límites + vecino compatible + conectado de verdad al inicio ──
  def validarColocacion(pos: Posicion, carta: CartaTunel): Boolean =
    if cuadricula.contains(pos)     then return false
    if !dentroDelTablero(pos)       then return false

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

    if !(validoArr && validoAba && validoIzq && validoDer) then return false

    // ── Regla clave: al menos un lado abierto de la carta nueva debe tocar
    //    una celda que esté REALMENTE conectada al inicio (no solo "presente").
    //    Así, si un derrumbe corta el camino, el lado huérfano deja de aceptar cartas
    //    hasta que alguien lo vuelva a unir a la red conectada al inicio.
    val conectados = componenteConectadoDesdeInicio
    (carta.arriba    && conectados.contains(pos.arriba))    ||
    (carta.abajo     && conectados.contains(pos.abajo))     ||
    (carta.izquierda && conectados.contains(pos.izquierda)) ||
    (carta.derecha   && conectados.contains(pos.derecha))

  // ── Eliminar carta (Derrumbe) ─────────────────────────────────────────────
  def eliminarCarta(pos: Posicion): Option[Tablero] =
    if pos == posicionInicio || posicionesMeta.contains(pos) then None
    else cuadricula.get(pos).map(_ => this.copy(cuadricula = cuadricula - pos))

  // ── Metas ocultas a las que ya llega un camino conectado desde el inicio ──
  // (independientemente de si son oro o carbón: con solo "llegar" se revelan)
  def metasOcultasAlcanzables: List[Posicion] =
    posicionesMeta.filter { p =>
      cuadricula.get(p).exists(c => c.esMeta && c.estaOculta) && hayConexion(posicionInicio, p)
    }

  // ── Revelar una meta concreta (queda visible para siempre, como en el juego real) ──
  def revelarMeta(pos: Posicion): Tablero =
    cuadricula.get(pos) match
      case Some(c) => this.copy(cuadricula = cuadricula + (pos -> c.copy(estaOculta = false)))
      case None    => this

  // ── BFS genérico desde una posición, devuelve TODO lo alcanzable ──────────
  private def bfsDesde(origen: Posicion): Set[Posicion] =
    def bfs(cola: List[Posicion], visitados: Set[Posicion]): Set[Posicion] =
      cola match
        case Nil => visitados
        case actual :: resto =>
          if visitados.contains(actual) then bfs(resto, visitados)
          else
            val vecinos = cuadricula.get(actual)
              .map(c => vecinosConectados(actual, c))
              .getOrElse(Nil)
              .filterNot(visitados.contains)
            bfs(resto ++ vecinos, visitados + actual)
    bfs(List(origen), Set.empty)

  def hayConexion(desde: Posicion, hasta: Posicion): Boolean =
    bfsDesde(desde).contains(hasta)

  // ── Todas las posiciones realmente conectadas al inicio ahora mismo ──────
  def componenteConectadoDesdeInicio: Set[Posicion] =
    bfsDesde(posicionInicio)

  private def vecinosConectados(pos: Posicion, carta: CartaTunel): List[Posicion] =
    // Las SC tienen lados visuales reales (para validar colocacion) pero
    // nunca propagan conexion en el BFS: son callejones ciegos para la red.
    if carta.esCallejonSinSalida then return Nil
    List(
      Option.when(carta.arriba)(pos.arriba),
      Option.when(carta.abajo)(pos.abajo),
      Option.when(carta.izquierda)(pos.izquierda),
      Option.when(carta.derecha)(pos.derecha)
    ).flatten.filter { vecPos =>
      cuadricula.get(vecPos).exists { vecCarta =>
        // Tampoco entrar en una SC desde el otro lado
        if vecCarta.esCallejonSinSalida then false
        else if vecPos == pos.arriba    then vecCarta.abajo
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
    mensajeAlerta: String = "",
    // Mensaje solo para el jugador que actuó (ej: resultado Mapa/Lupa).
    // El servidor lo limpia antes de retransmitir el estado a los demás clientes.
    mensajePrivado: String = ""
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
  // voltear=true aplica la rotación 180° antes de validar y colocar
  def colocarTunel(cartaId: Int, pos: Posicion, voltear: Boolean = false): ResultadoAccion =
    val jugador = jugadorActual

    if jugador.estaBloqueado then
      return ResultadoAccion.Error(s"${jugador.nombre} tiene herramientas rotas y no puede colocar túneles.")

    jugador.mano.collectFirst { case c: CartaTunel if c.id == cartaId => c } match
      case None => ResultadoAccion.Error("Esa carta de túnel no está en tu mano.")
      case Some(cartaOriginal) =>
        // Aplicar volteo si el jugador lo pidió
        val carta = if voltear then cartaOriginal.voltear else cartaOriginal
        if !tablero.validarColocacion(pos, carta) then
          ResultadoAccion.Error("Posición inválida: la carta no conecta correctamente.")
        else
          val nuevoTablero = tablero.copy(cuadricula = tablero.cuadricula + (pos -> carta))

          // ── ¿El nuevo túnel conecta con alguna meta todavía oculta? ────────
          // Si llega a una, se revela (oro o carbón). Si es oro, termina la partida.
          val metasAlcanzadas = nuevoTablero.metasOcultasAlcanzables

          val (tableroFinal, msg, estado) =
            if metasAlcanzadas.isEmpty then
              (nuevoTablero, "", EstadoPartida.EnCurso)
            else
              val tableroRevelado = metasAlcanzadas.foldLeft(nuevoTablero)((t, p) => t.revelarMeta(p))
              val metaOro = metasAlcanzadas.find(p => nuevoTablero.cuadricula.get(p).exists(_.esOro))

              metaOro match
                case Some(_) =>
                  (tableroRevelado, s"¡¡${jugador.nombre} y los Buscadores encontraron el ORO!!", EstadoPartida.GanoBuscadores)
                case None =>
                  val descripciones = metasAlcanzadas.map(p => s"(${p.x},${p.y}) → Carbón")
                  val mensajeRevelacion = s"${jugador.nombre} llegó a una meta falsa: ${descripciones.mkString(", ")}."
                  (tableroRevelado, mensajeRevelacion, EstadoPartida.EnCurso)

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
                  val msg = s"¡${jugador.nombre} saboteó el $herramienta de ${objetivo.nombre}!"
                  val juegoBase = avanzarTurno(jugador, cartaId, tablero, EstadoPartida.EnCurso, msg)
                  // Aplicar el cambio de herramienta sobre juegoBase (ya tiene la carta eliminada y nueva robada)
                  val nuevosJugadores = juegoBase.listaJugadores.map(j => if j.id == jugadorObjetivoId then objetivoActualizado else j)
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
                    val msg = s"${jugador.nombre} reparó una herramienta de ${objetivo.nombre}."
                    val juegoBase = avanzarTurno(jugador, cartaId, tablero, EstadoPartida.EnCurso, msg)
                    // Aplicar la reparación sobre juegoBase (ya tiene la carta eliminada y nueva robada)
                    val nuevosJugadores = juegoBase.listaJugadores.map(j => if j.id == jugadorObjetivoId then objetivoReparado else j)
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
                // msgPublico informa a todos que alguien usó la lupa (sin revelar el contenido).
                // msgPrivado solo llega al jugador que la usó (ver red.scala).
                val msgPublico  = s"${jugador.nombre} usó la Lupa en (${posicionMeta.x},${posicionMeta.y})."
                val msgPrivado  = s"[Solo tú] Meta (${posicionMeta.x},${posicionMeta.y}): $contenido"
                val juegoAvanzado = avanzarTurno(jugador, cartaId, tablero, EstadoPartida.EnCurso, msgPublico)
                ResultadoAccion.Exito(juegoAvanzado.copy(mensajePrivado = msgPrivado), msgPrivado)
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

    val tuneles: List[CartaTunel] = List.concat(
      // ── Cruce de 4 vías (simétrico, no se gira) ──────────────────────────
      List.tabulate(5) (_ => CartaTunel(nextId(), "Cruce",
        arriba=true,  abajo=true,  izquierda=true,  derecha=true,  esCallejonSinSalida=false)),

      // ── Rectas (simétricas al girar 180°, no se giran) ───────────────────
      List.tabulate(8) (_ => CartaTunel(nextId(), "Túnel Recto H",
        arriba=false, abajo=false, izquierda=true,  derecha=true,  esCallejonSinSalida=false)),
      List.tabulate(8) (_ => CartaTunel(nextId(), "Túnel Recto V",
        arriba=true,  abajo=true,  izquierda=false, derecha=false, esCallejonSinSalida=false)),

      // ── Cruces en T: 2 tipos, volteables 180° ─────────────────────────────
      // "sin arriba"  girado 180° → queda "sin abajo"      (imagen: Carta_Camino3)
      List.tabulate(8) (_ => CartaTunel(nextId(), "Cruce en T (sin arriba)",
        arriba=false, abajo=true,  izquierda=true,  derecha=true,  esCallejonSinSalida=false)),
      // "sin derecha" girado 180° → queda "sin izquierda"  (imagen: Carta_Camino5)
      List.tabulate(8) (_ => CartaTunel(nextId(), "Cruce en T (sin derecha)",
        arriba=true,  abajo=true,  izquierda=true,  derecha=false, esCallejonSinSalida=false)),

      // ── Codos: 2 tipos, volteables 180° ──────────────────────────────────
      // "arriba-izq"  girado 180° → queda "abajo-der"      (imagen: Carta_Camino4)
      List.tabulate(6) (_ => CartaTunel(nextId(), "Curva (arriba-izquierda)",
        arriba=true,  abajo=false, izquierda=true,  derecha=false, esCallejonSinSalida=false)),
      // "abajo-izq"   girado 180° → queda "arriba-der"     (imagen: Carta_Camino6)
      List.tabulate(6) (_ => CartaTunel(nextId(), "Curva (abajo-izquierda)",
        arriba=false, abajo=true,  izquierda=true,  derecha=false, esCallejonSinSalida=false)),

      // ── Callejones con 1 vía: 2 tipos, volteables 180° ───────────────────
      // "solo izquierda" girado 180° → queda "solo derecha"  (imagen: SinCamino7)
      List.tabulate(4) (_ => CartaTunel(nextId(), "Callejón (solo izquierda)",
        arriba=false, abajo=false, izquierda=true,  derecha=false, esCallejonSinSalida=true)),
      // "solo abajo"     girado 180° → queda "solo arriba"   (imagen: SinCamino9)
      List.tabulate(4) (_ => CartaTunel(nextId(), "Callejón (solo abajo)",
        arriba=false, abajo=true,  izquierda=false, derecha=false, esCallejonSinSalida=true)),

      // ── SC (Sin Camino): lados visuales reales, pero el BFS las ignora ──────
      // Simétricos (no volteables) — misma forma que sus equivalentes con camino
      List.tabulate(3) (_ => CartaTunel(nextId(), "SC Cruce",
        arriba=true,  abajo=true,  izquierda=true,  derecha=true,  esCallejonSinSalida=true)),
      List.tabulate(3) (_ => CartaTunel(nextId(), "SC Recto H",
        arriba=false, abajo=false, izquierda=true,  derecha=true,  esCallejonSinSalida=true)),
      List.tabulate(3) (_ => CartaTunel(nextId(), "SC Recto V",
        arriba=true,  abajo=true,  izquierda=false, derecha=false, esCallejonSinSalida=true)),
      // Volteables: mismos lados que sus equivalentes con camino
      List.tabulate(6) (_ => CartaTunel(nextId(), "SC Cruce en T (sin arriba)",
        arriba=false, abajo=true,  izquierda=true,  derecha=true,  esCallejonSinSalida=true)),
      List.tabulate(6) (_ => CartaTunel(nextId(), "SC Cruce en T (sin der)",
        arriba=true,  abajo=true,  izquierda=true,  derecha=false, esCallejonSinSalida=true)),
      List.tabulate(6) (_ => CartaTunel(nextId(), "SC Curva (arriba-izq)",
        arriba=true,  abajo=false, izquierda=true,  derecha=false, esCallejonSinSalida=true)),
      List.tabulate(6) (_ => CartaTunel(nextId(), "SC Curva (abajo-izq)",
        arriba=false, abajo=true,  izquierda=true,  derecha=false, esCallejonSinSalida=true))
    )

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