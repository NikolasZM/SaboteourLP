// ─────────────────────────────────────────────────────────────────────────────
//  Red.scala  —  Protocolo de red para Saboteur (LAN, TCP, host/cliente)
//
//  Arquitectura:
//    • Host : tiene el Juego canónico. Recibe acciones, las procesa, y
//              retransmite el nuevo estado a todos los clientes.
//    • Cliente: solo tiene la vista. Envía acciones al host y recibe
//               el Juego actualizado para renderizarlo.
//
//  Transporte : TCP puro (java.net), un mensaje JSON por línea (\n).
//  Serialización: JSON manual — sin librerías externas.
//
//  Modelo funcional de red:
//    Las únicas dos funciones de efecto son:
//      enviar(out, msg)          — escribe una línea en el socket
//      recibirLoop(in, handler)  — bucle que llama handler(msg) por cada línea
//    Todo lo demás (parseo, construcción de mensajes, lógica) es puro.
// ─────────────────────────────────────────────────────────────────────────────

import java.net.{ServerSocket, Socket, InetAddress}
import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import scala.util.{Try, Success, Failure}
import scalafx.application.Platform

// ─────────────────────────────────────────────
//  MENSAJES DE RED  (ADT sellado, inmutable)
// ─────────────────────────────────────────────

sealed trait MensajeRed

// Cliente → Host
case class AccionColocarTunel(cartaId: Int, posX: Int, posY: Int, voltear: Boolean) extends MensajeRed
case class AccionSabotaje(cartaId: Int, objetivoId: Int)                             extends MensajeRed
case class AccionReparacion(cartaId: Int, objetivoId: Int)                           extends MensajeRed
case class AccionMapa(cartaId: Int, posX: Int, posY: Int)                            extends MensajeRed
case class AccionDerrumbe(cartaId: Int, posX: Int, posY: Int)                        extends MensajeRed
case class AccionDescartar(cartaId: Int)                                             extends MensajeRed

// Host → todos los clientes
case class EstadoJuegoMsg(juego: Juego)    extends MensajeRed   // broadcast del nuevo estado
case class ErrorMsg(razon: String)         extends MensajeRed   // rebote de error al que actuó
case class BienvenidaMsg(jugadorId: Int)   extends MensajeRed   // asigna el id al cliente

// ─────────────────────────────────────────────
//  SERIALIZACIÓN JSON  (manual, pura)
// ─────────────────────────────────────────────

object Json:
  // ── Helpers de construcción ──────────────────────────────────────────────
  def obj(campos: (String, String)*): String =
    campos.map { case (k, v) => s""""$k":$v""" }.mkString("{", ",", "}")

  def arr(elems: Iterable[String]): String =
    elems.mkString("[", ",", "]")

  def str(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def bool(b: Boolean): String = if b then "true" else "false"
  def num(n: Int): String = n.toString

  // ── Serializar dominio ───────────────────────────────────────────────────
  def serHerramienta(h: Herramienta): String = str(h.toString)

  def serTipoAccion(t: TipoAccion): String = t match
    case TipoAccion.SABOTAJE(h)    => obj("tipo" -> str("SABOTAJE"),    "herramienta" -> serHerramienta(h))
    case TipoAccion.REPARACION(hs) => obj("tipo" -> str("REPARACION"),  "herramientas" -> arr(hs.map(serHerramienta)))
    case TipoAccion.MAPA           => obj("tipo" -> str("MAPA"))
    case TipoAccion.DERRUMBE       => obj("tipo" -> str("DERRUMBE"))

  def serCarta(c: Carta): String = c match
    case t: CartaTunel =>
      obj(
        "tipo"                -> str("tunel"),
        "id"                  -> num(t.id),
        "nombre"              -> str(t.nombre),
        "arriba"              -> bool(t.arriba),
        "abajo"               -> bool(t.abajo),
        "izquierda"           -> bool(t.izquierda),
        "derecha"             -> bool(t.derecha),
        "esCallejonSinSalida" -> bool(t.esCallejonSinSalida),
        "esMeta"              -> bool(t.esMeta),
        "estaOculta"          -> bool(t.estaOculta),
        "esOro"               -> bool(t.esOro),
        "imagenVolteada"      -> bool(t.imagenVolteada)
      )
    case a: CartaAccion =>
      obj(
        "tipo"       -> str("accion"),
        "id"         -> num(a.id),
        "nombre"     -> str(a.nombre),
        "tipoEfecto" -> serTipoAccion(a.tipoEfecto)
      )

  def serJugador(j: Jugador): String =
    obj(
      "id"                -> num(j.id),
      "nombre"            -> str(j.nombre),
      "rol"               -> str(j.rol.toString),
      "mano"              -> arr(j.mano.map(serCarta)),
      "herramientasRotas" -> arr(j.herramientasRotas.map(serHerramienta))
    )

  def serPosicion(p: Posicion): String =
    obj("x" -> num(p.x), "y" -> num(p.y))

  def serEntradaCuadricula(pos: Posicion, carta: CartaTunel): String =
    obj("pos" -> serPosicion(pos), "carta" -> serCarta(carta))

  def serTablero(t: Tablero): String =
    obj(
      "cuadricula"     -> arr(t.cuadricula.map { case (p, c) => serEntradaCuadricula(p, c) }),
      "posicionInicio" -> serPosicion(t.posicionInicio),
      "posicionesMeta" -> arr(t.posicionesMeta.map(serPosicion))
    )

  def serMazo(m: Mazo): String =
    // Los clientes no necesitan ver el contenido del mazo — solo el tamaño
    obj(
      "cantidadRobo"     -> num(m.cartasRobo.size),
      "cantidadDescarte" -> num(m.cartasDescarte.size)
    )

  def serJuego(j: Juego): String =
    obj(
      "listaJugadores"      -> arr(j.listaJugadores.map(serJugador)),
      "tablero"             -> serTablero(j.tablero),
      "mazo"                -> serMazo(j.mazo),
      "turnoActual"         -> num(j.turnoActual),
      "indiceJugadorActual" -> num(j.indiceJugadorActual),
      "estadoPartida"       -> str(j.estadoPartida.toString),
      "mensajeAlerta"       -> str(j.mensajeAlerta)
    )

  def serMensaje(m: MensajeRed): String = m match
    case AccionColocarTunel(cId, px, py, v) =>
      obj("cmd" -> str("COLOCAR"), "cartaId" -> num(cId),
          "posX" -> num(px), "posY" -> num(py), "voltear" -> bool(v))
    case AccionSabotaje(cId, oId) =>
      obj("cmd" -> str("SABOTAJE"), "cartaId" -> num(cId), "objetivoId" -> num(oId))
    case AccionReparacion(cId, oId) =>
      obj("cmd" -> str("REPARACION"), "cartaId" -> num(cId), "objetivoId" -> num(oId))
    case AccionMapa(cId, px, py) =>
      obj("cmd" -> str("MAPA"), "cartaId" -> num(cId), "posX" -> num(px), "posY" -> num(py))
    case AccionDerrumbe(cId, px, py) =>
      obj("cmd" -> str("DERRUMBE"), "cartaId" -> num(cId), "posX" -> num(px), "posY" -> num(py))
    case AccionDescartar(cId) =>
      obj("cmd" -> str("DESCARTAR"), "cartaId" -> num(cId))
    case EstadoJuegoMsg(j) =>
      obj("cmd" -> str("ESTADO"), "juego" -> serJuego(j))
    case ErrorMsg(r) =>
      obj("cmd" -> str("ERROR"), "razon" -> str(r))
    case BienvenidaMsg(id) =>
      obj("cmd" -> str("BIENVENIDA"), "jugadorId" -> num(id))

// ─────────────────────────────────────────────
//  PARSEO JSON  (manual, puro)
//  Parser minimalista para el subconjunto que
//  usamos. No es un parser JSON general.
// ─────────────────────────────────────────────

object Parser:
  // ── Extraer valor de una clave en JSON plano ──────────────────────────────
  // Devuelve el valor crudo (puede ser string con comillas, número, objeto, array)
  def campo(json: String, clave: String): Option[String] =
    val patron = s""""$clave":"""
    val idx = json.indexOf(patron)
    if idx < 0 then None
    else
      val inicio = idx + patron.length
      Some(extraerValor(json, inicio))

  // Extrae el valor en la posición dada (objeto, array, string o escalar)
  private def extraerValor(s: String, desde: Int): String =
    val c = s(desde)
    c match
      case '{' => extraerBalanceado(s, desde, '{', '}')
      case '[' => extraerBalanceado(s, desde, '[', ']')
      case '"' => extraerString(s, desde)
      case _   => // número, bool, null
        val fin = s.indexWhere(c => c == ',' || c == '}' || c == ']', desde)
        if fin < 0 then s.substring(desde) else s.substring(desde, fin)

  private def extraerBalanceado(s: String, desde: Int, abre: Char, cierra: Char): String =
    def go(i: Int, nivel: Int, enStr: Boolean, escape: Boolean): Int =
      if i >= s.length then s.length
      else if escape then go(i + 1, nivel, enStr, false)
      else s(i) match
        case '\\' if enStr  => go(i + 1, nivel, enStr, true)
        case '"'            => go(i + 1, nivel, !enStr, false)
        case c if !enStr && c == abre  => go(i + 1, nivel + 1, enStr, false)
        case c if !enStr && c == cierra =>
          if nivel == 1 then i + 1 else go(i + 1, nivel - 1, enStr, false)
        case _              => go(i + 1, nivel, enStr, false)
    s.substring(desde, go(desde, 0, false, false))

  private def extraerString(s: String, desde: Int): String =
    // incluye las comillas
    def go(i: Int, escape: Boolean): Int =
      if i >= s.length then s.length
      else if escape then go(i + 1, false)
      else s(i) match
        case '\\' => go(i + 1, true)
        case '"'  => i + 1
        case _    => go(i + 1, false)
    s.substring(desde, go(desde + 1, false))

  // Quita comillas de un string JSON
  def quitarComillas(s: String): String =
    if s.startsWith("\"") && s.endsWith("\"") then
      s.substring(1, s.length - 1)
        .replace("\\\"", "\"")
        .replace("\\\\", "\\")
    else s

  // Divide un array JSON en sus elementos
  def elemArray(arr: String): List[String] =
    val interior = arr.stripPrefix("[").stripSuffix("]").trim
    if interior.isEmpty then Nil
    else
      def go(s: String, acc: List[String]): List[String] =
        val s2 = s.trim
        if s2.isEmpty then acc.reverse
        else
          val elem = extraerValor(s2, 0)
          val resto = s2.drop(elem.length).dropWhile(c => c == ',' || c == ' ')
          go(resto, elem :: acc)
      go(interior, Nil)

  // ── Parsear tipos de dominio ──────────────────────────────────────────────
  def parsHerramienta(s: String): Option[Herramienta] =
    quitarComillas(s) match
      case "PICO"        => Some(Herramienta.PICO)
      case "CARRETILLA"  => Some(Herramienta.CARRETILLA)
      case "FAROL"       => Some(Herramienta.FAROL)
      case _             => None

  def parsTipoAccion(json: String): Option[TipoAccion] =
    campo(json, "tipo").map(quitarComillas) match
      case Some("SABOTAJE")  =>
        campo(json, "herramienta").flatMap(parsHerramienta).map(TipoAccion.SABOTAJE.apply)
      case Some("REPARACION") =>
        campo(json, "herramientas").map { arrJson =>
          val hs = elemArray(arrJson).flatMap(parsHerramienta)
          TipoAccion.REPARACION(hs)
        }
      case Some("MAPA")      => Some(TipoAccion.MAPA)
      case Some("DERRUMBE")  => Some(TipoAccion.DERRUMBE)
      case _                 => None

  def parsCarta(json: String): Option[Carta] =
    campo(json, "tipo").map(quitarComillas) match
      case Some("tunel") =>
        for
          id     <- campo(json, "id").map(_.toInt)
          nombre <- campo(json, "nombre").map(quitarComillas)
          arriba <- campo(json, "arriba").map(_ == "true")
          abajo  <- campo(json, "abajo").map(_ == "true")
          izq    <- campo(json, "izquierda").map(_ == "true")
          der    <- campo(json, "derecha").map(_ == "true")
          esc    <- campo(json, "esCallejonSinSalida").map(_ == "true")
          esMeta <- campo(json, "esMeta").map(_ == "true")
          oculta <- campo(json, "estaOculta").map(_ == "true")
          esOro  <- campo(json, "esOro").map(_ == "true")
          imgVol <- campo(json, "imagenVolteada").map(_ == "true")
        yield CartaTunel(id, nombre, arriba, abajo, izq, der, esc, esMeta, oculta, esOro, imgVol)
      case Some("accion") =>
        for
          id         <- campo(json, "id").map(_.toInt)
          nombre     <- campo(json, "nombre").map(quitarComillas)
          tipoJson   <- campo(json, "tipoEfecto")
          tipoEfecto <- parsTipoAccion(tipoJson)
        yield CartaAccion(id, nombre, tipoEfecto)
      case _ => None

  def parsJugador(json: String): Option[Jugador] =
    for
      id     <- campo(json, "id").map(_.toInt)
      nombre <- campo(json, "nombre").map(quitarComillas)
      rolStr <- campo(json, "rol").map(quitarComillas)
      rol    = if rolStr == "BUSCADOR" then Rol.BUSCADOR else Rol.SABOTEADOR
      manoArr <- campo(json, "mano")
      mano    = elemArray(manoArr).flatMap(parsCarta)
      herArr  <- campo(json, "herramientasRotas")
      hers    = elemArray(herArr).flatMap(parsHerramienta)
    yield Jugador(id, nombre, rol, mano, hers)

  def parsPosicion(json: String): Option[Posicion] =
    for
      x <- campo(json, "x").map(_.toInt)
      y <- campo(json, "y").map(_.toInt)
    yield Posicion(x, y)

  def parsTablero(json: String): Option[Tablero] =
    for
      cuadJson  <- campo(json, "cuadricula")
      inicioJ   <- campo(json, "posicionInicio")
      metasJ    <- campo(json, "posicionesMeta")
      inicio    <- parsPosicion(inicioJ)
      metas     = elemArray(metasJ).flatMap(parsPosicion)
      entradas  = elemArray(cuadJson).flatMap { e =>
        for
          posJ  <- campo(e, "pos")
          cartJ <- campo(e, "carta")
          pos   <- parsPosicion(posJ)
          cart  <- parsCarta(cartJ)
          tunel <- cart match { case t: CartaTunel => Some(t); case _ => None }
        yield pos -> tunel
      }
    yield Tablero(entradas.toMap, inicio, metas)

  def parsEstadoPartida(s: String): EstadoPartida = s match
    case "GanoBuscadores"   => EstadoPartida.GanoBuscadores
    case "GanoSaboteadores" => EstadoPartida.GanoSaboteadores
    case _                  => EstadoPartida.EnCurso

  // El mazo llega solo con cantidades, lo reconstruimos vacío con esos tamaños
  def parsJuego(json: String): Option[Juego] =
    for
      jugJson  <- campo(json, "listaJugadores")
      tabJson  <- campo(json, "tablero")
      mazoJson <- campo(json, "mazo")
      turno    <- campo(json, "turnoActual").map(_.toInt)
      indice   <- campo(json, "indiceJugadorActual").map(_.toInt)
      estadoS  <- campo(json, "estadoPartida").map(quitarComillas)
      alerta   <- campo(json, "mensajeAlerta").map(quitarComillas)
      jugadores = elemArray(jugJson).flatMap(parsJugador)
      tablero  <- parsTablero(tabJson)
      cantRobo <- campo(mazoJson, "cantidadRobo").map(_.toInt)
    yield
      // El mazo en el cliente es opaco: solo importa el tamaño para mostrarlo
      val mazoOpaco = Mazo(
        cartasRobo    = List.fill(cantRobo)(CartaAccion(-999, "?", TipoAccion.MAPA)),
        cartasDescarte = Nil
      )
      Juego(jugadores, tablero, mazoOpaco, turno, indice, parsEstadoPartida(estadoS), alerta)

  def parsMensaje(linea: String): Option[MensajeRed] =
    campo(linea, "cmd").map(quitarComillas) match
      case Some("COLOCAR") =>
        for
          cId <- campo(linea, "cartaId").map(_.toInt)
          px  <- campo(linea, "posX").map(_.toInt)
          py  <- campo(linea, "posY").map(_.toInt)
          v   <- campo(linea, "voltear").map(_ == "true")
        yield AccionColocarTunel(cId, px, py, v)
      case Some("SABOTAJE") =>
        for
          cId <- campo(linea, "cartaId").map(_.toInt)
          oId <- campo(linea, "objetivoId").map(_.toInt)
        yield AccionSabotaje(cId, oId)
      case Some("REPARACION") =>
        for
          cId <- campo(linea, "cartaId").map(_.toInt)
          oId <- campo(linea, "objetivoId").map(_.toInt)
        yield AccionReparacion(cId, oId)
      case Some("MAPA") =>
        for
          cId <- campo(linea, "cartaId").map(_.toInt)
          px  <- campo(linea, "posX").map(_.toInt)
          py  <- campo(linea, "posY").map(_.toInt)
        yield AccionMapa(cId, px, py)
      case Some("DERRUMBE") =>
        for
          cId <- campo(linea, "cartaId").map(_.toInt)
          px  <- campo(linea, "posX").map(_.toInt)
          py  <- campo(linea, "posY").map(_.toInt)
        yield AccionDerrumbe(cId, px, py)
      case Some("DESCARTAR") =>
        campo(linea, "cartaId").map(_.toInt).map(AccionDescartar.apply)
      case Some("ESTADO") =>
        campo(linea, "juego").flatMap(parsJuego).map(EstadoJuegoMsg.apply)
      case Some("ERROR") =>
        campo(linea, "razon").map(quitarComillas).map(ErrorMsg.apply)
      case Some("BIENVENIDA") =>
        campo(linea, "jugadorId").map(_.toInt).map(BienvenidaMsg.apply)
      case _ => None

// ─────────────────────────────────────────────
//  TRANSPORTE  (efectos aislados)
//  Las dos únicas funciones con side effects de red.
// ─────────────────────────────────────────────

object Transporte:
  def enviar(out: PrintWriter, msg: MensajeRed): Unit =
    out.println(Json.serMensaje(msg))
    out.flush()

  // Lanza un hilo daemon que llama handler por cada línea recibida.
  // Cuando la conexión se cierra llama onCierre.
  def recibirLoop(
    in:       BufferedReader,
    handler:  MensajeRed => Unit,
    onCierre: () => Unit
  ): Unit =
    val hilo = new Thread(() =>
      def loop(): Unit =
        val linea = Try(in.readLine())
        linea match
          case Success(null) | Failure(_) => onCierre()
          case Success(l) if l.trim.nonEmpty =>
            Parser.parsMensaje(l.trim).foreach(handler)
            loop()
          case _ => loop()
      loop()
    )
    hilo.setDaemon(true)
    hilo.start()

// ─────────────────────────────────────────────
//  MOTOR DE ACCIONES — compartido por el host y el modo local
//  Función pura: Juego × Mensaje × jugadorId → ResultadoAccion
//  Verifica el turno y despacha a la operación correspondiente de Juego.
// ─────────────────────────────────────────────

def aplicarAccionAJuego(juego: Juego, msg: MensajeRed, jugadorId: Int): ResultadoAccion =
  if juego.jugadorActual.id != jugadorId then
    ResultadoAccion.Error(s"No es tu turno (turno de ${juego.jugadorActual.nombre}).")
  else msg match
    case AccionColocarTunel(cId, px, py, v) => juego.colocarTunel(cId, Posicion(px, py), v)
    case AccionSabotaje(cId, oId)            => juego.aplicarSabotaje(cId, oId)
    case AccionReparacion(cId, oId)          => juego.aplicarReparacion(cId, oId)
    case AccionMapa(cId, px, py)             => juego.usarMapa(cId, Posicion(px, py))
    case AccionDerrumbe(cId, px, py)         => juego.aplicarDerrumbe(cId, Posicion(px, py))
    case AccionDescartar(cId)                => juego.descartarCarta(cId)
    case _                                   => ResultadoAccion.Error("Mensaje no reconocido.")

// ─────────────────────────────────────────────
//  CONTEXTO DE RED PARA LA UI
//  Si es None la partida es local (hotseat, un solo proceso).
//  Si es Some, exactamente uno de los dos campos está definido:
//    servidor -> soy el host (el mismo proceso sirve y juega)
//    cliente  -> soy un cliente remoto conectado al host
// ─────────────────────────────────────────────

case class ContextoRed(
  miId:     Int,
  servidor: Option[ServidorJuego] = None,
  cliente:  Option[ClienteJuego]  = None
)

// ─────────────────────────────────────────────
//  SERVIDOR (HOST)
//  Estado: el Juego canónico vive aquí.
//  La única mutación permitida es el ref al Juego
//  actual, protegida por sincronización.
//  Todo lo demás es puro.
// ─────────────────────────────────────────────

class ServidorJuego(puerto: Int, juegoInicial: Juego):

  // El host siempre es el primer jugador de la lista
  private val hostId: Int = juegoInicial.listaJugadores.head.id

  // El estado canónico del juego. Se reemplaza atómicamente en cada acción.
  // Es el único punto de mutación de toda la aplicación de red.
  private var estadoActual: Juego = juegoInicial

  // Lista de salidas activas (una por CLIENTE REMOTO conectado; el host no está aquí)
  private var clientes: List[(Int, PrintWriter)] = Nil   // (jugadorId, out)

  // Callback para refrescar la UI del host cuando un cliente remoto juega
  private var onEstadoCambiadoHost: Option[Juego => Unit] = None

  /** La UI del host llama a esto para recibir actualizaciones de turno remoto. */
  def registrarUIHost(cb: Juego => Unit): Unit =
    onEstadoCambiadoHost = Some(cb)

  // ── Arrancar el servidor en un hilo daemon ───────────────────────────────
  def iniciar(onClienteConectado: Int => Unit): Unit =
    val hilo = new Thread(() =>
      val servidor = new ServerSocket(puerto)
      println(s"[HOST] Escuchando en puerto $puerto")
      esperarClientes(servidor, onClienteConectado)
    )
    hilo.setDaemon(true)
    hilo.start()

  // Recursión en lugar de while loop
  private def esperarClientes(servidor: ServerSocket, onConectado: Int => Unit): Unit =
    val socket  = servidor.accept()
    val out     = new PrintWriter(socket.getOutputStream, true)
    val in      = new BufferedReader(new InputStreamReader(socket.getInputStream))

    // Asignar jugador: el próximo sin conexión asignada, saltando al host
    // (los jugadores ya existen en juegoInicial, solo los conectamos en orden)
    val jugadorId = this.synchronized {
      val idAsignado = estadoActual.listaJugadores
        .map(_.id)
        .filterNot(_ == hostId)          // el host (id=1) NO está disponible para clientes remotos
        .find(id => !clientes.map(_._1).contains(id))
        .getOrElse(-1)
      clientes = clientes :+ (idAsignado, out)
      idAsignado
    }

    println(s"[HOST] Jugador $jugadorId conectado desde ${socket.getInetAddress}")

    // Mandar bienvenida + estado actual al recién conectado
    Transporte.enviar(out, BienvenidaMsg(jugadorId))
    Transporte.enviar(out, EstadoJuegoMsg(estadoActual))

    onConectado(jugadorId)

    // Escuchar acciones de este cliente
    Transporte.recibirLoop(
      in,
      msg => Platform.runLater { procesarAccion(msg, jugadorId) },
      ()  => println(s"[HOST] Jugador $jugadorId desconectado")
    )

    esperarClientes(servidor, onConectado)

  def clientesConectados: Int = this.synchronized(clientes.size)

  // Snapshot del estado canónico (por si la UI del host lo necesita sin pasar por una acción)
  def estadoActualSnapshot: Juego = this.synchronized(estadoActual)

  // ── Procesa una acción y devuelve el resultado AL INSTANTE ────────────────
  // Esta es la única vía de mutación del estado canónico. La usa tanto:
  //   • la UI del host, llamándola directamente (síncrono, mismo proceso)
  //   • el hilo de red, para las acciones que llegan de clientes remotos
  // Efecto: si tiene éxito, actualiza el estado y retransmite a TODOS los
  // clientes conectados (el host se entera por el valor de retorno).
  // notificarHostUI=true solo cuando la llamada viene de un cliente remoto:
  // dispara el callback de la UI del host para que refresque el tablero.
  def procesarAccionHost(msg: MensajeRed, jugadorId: Int,
                         notificarHostUI: Boolean = false): ResultadoAccion =
    this.synchronized:
      val resultado = aplicarAccionAJuego(estadoActual, msg, jugadorId)
      resultado match
        case ResultadoAccion.Exito(nuevoJuego, _) =>
          estadoActual = nuevoJuego
          clientes.foreach { case (_, out) =>
            Transporte.enviar(out, EstadoJuegoMsg(nuevoJuego))
          }
          // Refrescar la UI del host si la acción vino de un cliente remoto
          if notificarHostUI then
            onEstadoCambiadoHost.foreach(_(nuevoJuego))
        case ResultadoAccion.Error(_) => ()
      resultado

  // Acción que llega por red desde un cliente remoto: igual procesamiento,
  // pero el error solo se devuelve al cliente que la mandó (por socket).
  // notificarHostUI=true para que el host refresque su pantalla.
  private def procesarAccion(msg: MensajeRed, jugadorId: Int): Unit =
    procesarAccionHost(msg, jugadorId, notificarHostUI = true) match
      case ResultadoAccion.Error(razon) =>
        this.synchronized {
          clientes.find(_._1 == jugadorId).foreach { case (_, out) =>
            Transporte.enviar(out, ErrorMsg(razon))
          }
        }
      case ResultadoAccion.Exito(_, _) => ()

  def ipLocal: String =
    Try(InetAddress.getLocalHost.getHostAddress).getOrElse("127.0.0.1")

// ─────────────────────────────────────────────
//  CLIENTE
//  No tiene estado de juego. Recibe el Juego
//  del host y lo pasa a la UI via callback.
// ─────────────────────────────────────────────

class ClienteJuego(host: String, puerto: Int):

  private var outOpt: Option[PrintWriter] = None

  // A dónde se reenvían los mensajes/errores entrantes en este momento.
  // Empiezan en no-op y se fijan en conectar(); se pueden REDIRIGIR después
  // con alRecibir/alPerderConexion (p.ej. al pasar de la pantalla de
  // "conectando…" a la pantalla de juego), sin tener que reabrir el socket.
  private var manejadorMensaje: MensajeRed => Unit = _ => ()
  private var manejadorError:   String => Unit     = _ => ()

  // Conectar y arrancar escucha. Llama onMensaje por cada mensaje recibido.
  def conectar(
    onMensaje: MensajeRed => Unit,
    onError:   String => Unit
  ): Unit =
    manejadorMensaje = onMensaje
    manejadorError   = onError
    val hilo = new Thread(() =>
      Try {
        val socket = new Socket(host, puerto)
        val out    = new PrintWriter(socket.getOutputStream, true)
        val in     = new BufferedReader(new InputStreamReader(socket.getInputStream))
        outOpt = Some(out)
        Transporte.recibirLoop(
          in,
          msg => Platform.runLater { manejadorMensaje(msg) },
          ()  => Platform.runLater { manejadorError("Conexión perdida con el host.") }
        )
      }.recover { case e => Platform.runLater { manejadorError(s"No se pudo conectar: ${e.getMessage}") } }
      ()
    )
    hilo.setDaemon(true)
    hilo.start()

  // Redirige los mensajes entrantes a otra función (sin reconectar).
  def alRecibir(nuevoManejador: MensajeRed => Unit): Unit =
    manejadorMensaje = nuevoManejador

  // Redirige los avisos de desconexión a otra función (sin reconectar).
  def alPerderConexion(nuevoManejador: String => Unit): Unit =
    manejadorError = nuevoManejador

  // Enviar una acción al host (solo si hay conexión)
  def enviarAccion(accion: MensajeRed): Unit =
    outOpt.foreach(out => Transporte.enviar(out, accion))