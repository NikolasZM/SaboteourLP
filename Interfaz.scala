//> using dep "org.scalafx::scalafx:20.0.0-R31"

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.layout.{Pane, VBox, HBox}
import scalafx.scene.text.{Text, Font, FontWeight}
import scalafx.scene.control.{Button, TextField, Label}
import scalafx.scene.input.MouseEvent
import scalafx.scene.image.{Image, ImageView}
import scalafx.geometry.{Pos, Insets}
import scalafx.Includes.*

// ─────────────────────────────────────────────
//  ESTADO DE UI — inmutable, sin vars
//  Todo lo que antes eran vars sueltos ahora
//  vive aquí. Cada acción produce un EstadoUI nuevo.
// ─────────────────────────────────────────────

case class EstadoUI(
  juego:                    Juego,
  cartaSeleccionada:        Option[Carta]      = None,
  mensajeError:             String             = "",
  jugadorObjetivoSel:       Option[Int]        = None,
  cartasVolteadas:          Map[Int, Boolean]  = Map.empty
):
  // Helpers puros que devuelven un EstadoUI nuevo
  def seleccionar(c: Carta): EstadoUI =
    this.copy(cartaSeleccionada = Some(c), jugadorObjetivoSel = None, mensajeError = "")

  def deseleccionar: EstadoUI =
    this.copy(cartaSeleccionada = None, jugadorObjetivoSel = None,
              mensajeError = "", cartasVolteadas = Map.empty)

  def conError(msg: String): EstadoUI =
    this.copy(mensajeError = msg)

  def conJuego(j: Juego): EstadoUI =
    this.copy(juego = j, cartaSeleccionada = None, jugadorObjetivoSel = None,
              mensajeError = "", cartasVolteadas = Map.empty)

  def voltearCarta(id: Int): EstadoUI =
    val actual = cartasVolteadas.getOrElse(id, false)
    this.copy(cartasVolteadas = cartasVolteadas + (id -> !actual))

  def seleccionarObjetivo(id: Int): EstadoUI =
    this.copy(jugadorObjetivoSel = Some(id))

object InterfazJuego extends JFXApp3:

  // ── Dimensiones ───────────────────────────────────────────────────────────
  val anchoCarta     = 90
  val altoCarta      = 60
  val anchoCartaMano = 127
  val altoCartaMano  = 85
  val offsetY        = 55
  val altoPanelInf   = 210
  val anchoVentana   = 1100
  val altoVentana    = 740

  // ── Estilos reutilizables ─────────────────────────────────────────────────
  def estiloBoton(color: String) =
    s"-fx-font-size: 14px; -fx-background-color: $color; -fx-text-fill: white; -fx-font-weight: bold; -fx-cursor: hand; -fx-background-radius: 6;"

  // =========================================================================
  //  MAPEO CARTA → ARCHIVO DE IMAGEN
  // =========================================================================
  def nombreImagen(carta: Carta): String = carta match
    case t: CartaTunel if t.estaOculta         => "Carro.png"
    case t: CartaTunel if t.esMeta && t.esOro  => "Carta_Oro.png"
    case t: CartaTunel if t.esMeta             => "Carta_Carbon.png"
    case t: CartaTunel => t.nombre match
      case "Inicio"                      => "Carta_Escalera.png"
      case "Cruce"                       => "Carta_Camino2.png"
      case "Túnel Recto H"               => "Carta_Camino1.png"
      case "Túnel Recto V"               => "Carta_Camino7.png"
      case "Cruce en T (sin arriba)"     => "Carta_Camino3.png"
      case "Cruce en T (sin derecha)"    => "Carta_Camino5.png"
      case "Curva (arriba-izquierda)"    => "Carta_Camino4.png"
      case "Curva (abajo-izquierda)"     => "Carta_Camino6.png"
      case "Callejón (solo izquierda)"   => "SinCamino7.png"
      case "Callejón (solo abajo)"       => "SinCamino9.png"
      case "SC Cruce"                    => "SinCamino2.png"
      case "SC Recto H"                  => "SinCamino1.png"
      case "SC Recto V"                  => "SinCamino8.png"
      case "SC Cruce en T (sin arriba)"  => "SinCamino3.png"
      case "SC Cruce en T (sin der)"     => "SinCamino5.png"
      case "SC Curva (arriba-izq)"       => "SinCamino4.png"
      case "SC Curva (abajo-izq)"        => "SinCamino6.png"
      case _                             => "reverso.png"
    case a: CartaAccion => a.tipoEfecto match
      case TipoAccion.SABOTAJE(Herramienta.PICO)        => "Carta_PicoRoto.png"
      case TipoAccion.SABOTAJE(Herramienta.CARRETILLA)  => "RomperCarro.png"
      case TipoAccion.SABOTAJE(Herramienta.FAROL)       => "RomperFaro.png"
      case TipoAccion.REPARACION(hs)
        if hs.toSet == Set(Herramienta.PICO)                                  => "Carta_Pico.png"
      case TipoAccion.REPARACION(hs)
        if hs.toSet == Set(Herramienta.CARRETILLA)                            => "Carro.png"
      case TipoAccion.REPARACION(hs)
        if hs.toSet == Set(Herramienta.FAROL)                                 => "Faro.png"
      case TipoAccion.REPARACION(hs)
        if hs.toSet == Set(Herramienta.PICO, Herramienta.CARRETILLA)         => "PicoyCarro.png"
      case TipoAccion.REPARACION(hs)
        if hs.toSet == Set(Herramienta.PICO, Herramienta.FAROL)              => "PicoyFaro.png"
      case TipoAccion.REPARACION(hs)
        if hs.toSet == Set(Herramienta.CARRETILLA, Herramienta.FAROL)        => "FaroyCarro.png"
      case TipoAccion.MAPA     => "Mapa.png"
      case TipoAccion.DERRUMBE => "Derrumbe.png"
      case _                   => "Faro.png"

  def crearImageView(carta: Carta, ancho: Double, alto: Double,
                     cartasVolteadas: Map[Int, Boolean]): ImageView =
    val img = new Image(s"file:Imagenes/imagenes_redimensionadas/${nombreImagen(carta)}", ancho, alto, false, true)
    val iv  = new ImageView(img):
      fitWidth      = ancho
      fitHeight     = alto
      preserveRatio = false
    val estaVolteada = carta match
      case t: CartaTunel => t.imagenVolteada || cartasVolteadas.getOrElse(carta.id, false)
      case _             => cartasVolteadas.getOrElse(carta.id, false)
    if estaVolteada then iv.rotate = 180
    iv

  // =========================================================================
  //  INICIO
  // =========================================================================
  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      title  = "Saboteur — En busca del oro"
      width  = anchoVentana
      height = altoVentana
      scene  = crearEscenaMenu()

  // =========================================================================
  //  ESCENA: MENÚ PRINCIPAL
  // =========================================================================
  def crearEscenaMenu(): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(25, 25, 25)

      val titulo = new Text("SABOTEUR"):
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 72)

      val subtitulo = new Text("En busca del oro"):
        fill = LightGray
        font = Font.font("Arial", FontWeight.Light, 20)

      val btnJugar = new Button("JUGAR"):
        prefWidth  = 220
        prefHeight = 55
        style      = estiloBoton("#2e7d32")
        onAction   = () => stage.scene = crearEscenaSeleccionModo()

      val btnSalir = new Button("SALIR"):
        prefWidth  = 220
        prefHeight = 55
        style      = estiloBoton("#c62828")
        onAction   = () => sys.exit(0)

      val layout = new VBox(25):
        alignment = Pos.Center
        children  = List(titulo, subtitulo, btnJugar, btnSalir)

      layout.prefWidth  = anchoVentana
      layout.prefHeight = altoVentana
      layout.style      = "-fx-background-color: #191919;"
      content           = layout

  // =========================================================================
  //  ESCENA: SELECCIÓN DE MODO (local / crear partida LAN / unirse a LAN)
  // =========================================================================
  def crearEscenaSeleccionModo(): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(25, 25, 25)

      val titulo = new Text("¿Cómo quieres jugar?"):
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 32)

      val btnLocal = new Button("LOCAL  (un solo PC, turnos)"):
        prefWidth  = 320
        prefHeight = 55
        style      = estiloBoton("#2e7d32")
        onAction   = () => stage.scene = crearEscenaConfiguracion()

      val btnHost = new Button("CREAR PARTIDA  (ser el host en LAN)"):
        prefWidth  = 320
        prefHeight = 55
        style      = estiloBoton("#1565c0")
        onAction   = () => stage.scene = crearEscenaConfigHost()

      val btnCliente = new Button("UNIRSE A PARTIDA  (conectarme por LAN)"):
        prefWidth  = 320
        prefHeight = 55
        style      = estiloBoton("#6a1b9a")
        onAction   = () => stage.scene = crearEscenaUnirseLAN()

      val btnVolver = new Button("VOLVER"):
        prefWidth  = 320
        prefHeight = 45
        style      = estiloBoton("#555555")
        onAction   = () => stage.scene = crearEscenaMenu()

      val layout = new VBox(20):
        alignment = Pos.Center
        children  = List(titulo, btnLocal, btnHost, btnCliente, btnVolver)

      layout.prefWidth  = anchoVentana
      layout.prefHeight = altoVentana
      layout.style      = "-fx-background-color: #191919;"
      content           = layout

  // =========================================================================
  //  ESCENA: CONFIGURAR PARTIDA COMO HOST (LAN)
  //  El host escribe los nombres de TODOS los jugadores (igual que en local).
  //  Importante: el host siempre juega como el Jugador 1, así que su nombre
  //  debe ir primero. Los demás se conectarán y ocuparán los demás puestos
  //  en el orden en que se conecten.
  // =========================================================================
  def crearEscenaConfigHost(): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(25, 25, 25)

      val titulo = new Text("Crear partida (Host LAN)"):
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 28)

      val numJugadoresProp = new javafx.beans.property.SimpleIntegerProperty(3)

      val camposNombre: List[TextField] = (1 to 10).map { i =>
        new TextField:
          promptText = s"Nombre Jugador $i"
          text       = s"Jugador $i"
          prefWidth  = 280
          style      = "-fx-font-size: 14px;"
          visible    = i <= 3
          managed    = i <= 3
      }.toList

      def actualizarCampos(n: Int): Unit =
        camposNombre.zipWithIndex.foreach { case (campo, i) =>
          campo.visible = i < n
          campo.managed = i < n
        }
        txtNum.text = s"$n jugadores"

      val txtNum = new Text(s"${numJugadoresProp.get} jugadores"):
        fill = White
        font = Font.font("Arial", 18)

      val btnMenos = new Button("−"):
        prefWidth = 50; prefHeight = 40
        style     = estiloBoton("#555555")
        onAction  = () =>
          val n = numJugadoresProp.get
          if n > 3 then numJugadoresProp.set(n - 1); actualizarCampos(n - 1)

      val btnMas = new Button("+"):
        prefWidth = 50; prefHeight = 40
        style     = estiloBoton("#555555")
        onAction  = () =>
          val n = numJugadoresProp.get
          if n < 10 then numJugadoresProp.set(n + 1); actualizarCampos(n + 1)

      val filaBotones = new HBox(20):
        alignment = Pos.Center
        children  = List(btnMenos, txtNum, btnMas)

      val txtPuertoLabel = new Text("Puerto TCP:"):
        fill = LightGray; font = Font.font("Arial", 13)

      val campoPuerto = new TextField:
        text      = "5000"
        prefWidth = 100
        style     = "-fx-font-size: 14px;"

      val filaPuerto = new HBox(10):
        alignment = Pos.Center
        children  = List(txtPuertoLabel, campoPuerto)

      val txtInfo = new Text("Tu nombre (Jugador 1) debe ir primero — tú serás el Jugador 1. Los demás se conectan después y ocupan los siguientes puestos en orden."):
        fill      = LightGray
        font      = Font.font("Arial", 12)
        wrappingWidth = 420

      val txtError = new Text(""):
        fill = OrangeRed
        font = Font.font("Arial", 12)

      val btnCrear = new Button("CREAR PARTIDA"):
        prefWidth  = 280
        prefHeight = 55
        style      = estiloBoton("#1565c0")
        onAction   = () =>
          val puertoOpt = campoPuerto.text.value.trim.toIntOption
          puertoOpt match
            case None =>
              txtError.text = "Puerto inválido."
            case Some(puerto) =>
              val n       = numJugadoresProp.get
              val nombres = camposNombre.take(n).map(_.text.value.trim)
                .map(nm => if nm.isEmpty then "Jugador" else nm)
              val juegoInicial = FabricaJuego.crearJuego(nombres)
              val servidor     = new ServidorJuego(puerto, juegoInicial)
              val miId         = juegoInicial.listaJugadores.head.id
              stage.scene = crearEscenaSalaEsperaHost(servidor, juegoInicial, miId, n, puerto)

      val btnVolver = new Button("VOLVER"):
        prefWidth  = 280
        prefHeight = 45
        style      = estiloBoton("#555555")
        onAction   = () => stage.scene = crearEscenaSeleccionModo()

      val layout = new VBox(14):
        alignment = Pos.Center
        padding   = Insets(30)
        style     = "-fx-background-color: #191919;"
        children  = List(titulo, filaBotones) ++ camposNombre ++
                    List(filaPuerto, txtInfo, txtError, btnCrear, btnVolver)

      layout.prefWidth  = anchoVentana
      layout.prefHeight = altoVentana
      content           = layout

  // =========================================================================
  //  ESCENA: SALA DE ESPERA DEL HOST
  //  Arranca el ServidorJuego y se queda mostrando cuántos jugadores se
  //  han conectado. El host entra a la partida cuando quiera (lo normal
  //  es esperar a que estén todos).
  // =========================================================================
  def crearEscenaSalaEsperaHost(servidor: ServidorJuego, juegoInicial: Juego, miId: Int, totalJugadores: Int, puerto: Int): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(25, 25, 25)
      val contenedor = new VBox(16):
        alignment = Pos.Center
        padding   = Insets(30)

      val titulo = new Text("Esperando jugadores…"):
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 30)

      val txtIp = new Text(s"Tu IP en la red local:  ${servidor.ipLocal}     Puerto: $puerto"):
        fill = LightGreen
        font = Font.font("Arial", FontWeight.Bold, 18)

      val txtAyuda = new Text("Dale esa IP y el puerto a los demás jugadores para que se unan desde 'UNIRSE A PARTIDA'."):
        fill = LightGray
        font = Font.font("Arial", 13)

      val txtConectados = new Text(""):
        fill = White
        font = Font.font("Arial", FontWeight.Bold, 20)

      val listaNombres = new VBox(6):
        alignment = Pos.Center

      def refrescar(conectados: Int): Unit =
        txtConectados.text = s"Conectados: $conectados / ${totalJugadores - 1}"
        listaNombres.children.clear()
        juegoInicial.listaJugadores.zipWithIndex.foreach { case (j, idx) =>
          val esHost  = j.id == miId
          val llegado = esHost || idx <= conectados // host(1) + los primeros 'conectados' en orden de id
          val txt = new Text(s"${if llegado then "✓" else "…"}  ${j.nombre}${if esHost then " (tú, host)" else ""}"):
            fill = if llegado then LightGreen else DimGray
            font = Font.font("Arial", 14)
          listaNombres.children.add(txt)
        }

      val btnIniciar = new Button("ENTRAR A LA PARTIDA"):
        prefWidth  = 280
        prefHeight = 55
        style      = estiloBoton("#2e7d32")
        onAction   = () =>
          val ctx = ContextoRed(miId = miId, servidor = Some(servidor))
          stage.scene = crearEscenaJuego(EstadoUI(juego = juegoInicial), Some(ctx))

      val btnVolver = new Button("VOLVER"):
        prefWidth  = 280
        prefHeight = 45
        style      = estiloBoton("#555555")
        onAction   = () => stage.scene = crearEscenaSeleccionModo()

      contenedor.children = List(titulo, txtIp, txtAyuda, txtConectados, listaNombres, btnIniciar, btnVolver)
      content = contenedor

      refrescar(0)
      servidor.iniciar(onClienteConectado = _ =>
        scalafx.application.Platform.runLater { refrescar(servidor.clientesConectados) }
      )

  // =========================================================================
  //  ESCENA: UNIRSE A UNA PARTIDA (CLIENTE LAN)
  // =========================================================================
  def crearEscenaUnirseLAN(): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(25, 25, 25)
      val contenedor = new VBox(16):
        alignment = Pos.Center
        padding   = Insets(30)

      val titulo = new Text("Unirse a una partida"):
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 30)

      val campoIp = new TextField:
        promptText = "IP del host (ej: 192.168.1.5)"
        prefWidth  = 280
        style      = "-fx-font-size: 14px;"

      val campoPuerto = new TextField:
        promptText = "Puerto (ej: 5000)"
        text       = "5000"
        prefWidth  = 280
        style      = "-fx-font-size: 14px;"

      val txtEstado = new Text(""):
        fill = LightGray
        font = Font.font("Arial", 13)

      val btnConectar = new Button("CONECTAR"):
        prefWidth  = 280
        prefHeight = 55
        style      = estiloBoton("#6a1b9a")

      val btnVolver = new Button("VOLVER"):
        prefWidth  = 280
        prefHeight = 45
        style      = estiloBoton("#555555")
        onAction   = () => stage.scene = crearEscenaSeleccionModo()

      btnConectar.onAction = () =>
        val ip        = campoIp.text.value.trim
        val puertoOpt = campoPuerto.text.value.trim.toIntOption
        (ip.nonEmpty, puertoOpt) match
          case (false, _) => txtEstado.text = "Escribe la IP del host."
          case (_, None)  => txtEstado.text = "Puerto inválido."
          case (true, Some(puerto)) =>
            txtEstado.text   = "Conectando…"
            btnConectar.disable = true
            val cliente   = new ClienteJuego(ip, puerto)
            var miIdRecibido: Option[Int] = None

            cliente.conectar(
              onMensaje = {
                case BienvenidaMsg(id) =>
                  miIdRecibido = Some(id)
                  txtEstado.text = s"Conectado. Esperando estado de la partida…"
                case EstadoJuegoMsg(j) =>
                  miIdRecibido match
                    case Some(miId) =>
                      val ctx = ContextoRed(miId = miId, cliente = Some(cliente))
                      stage.scene = crearEscenaJuego(EstadoUI(juego = j), Some(ctx))
                    case None =>
                      txtEstado.text = "Estado recibido sin bienvenida — reintenta."
                case ErrorMsg(razon) =>
                  txtEstado.text = s"⚠ $razon"
                case _ => ()
              },
              onError = razon =>
                txtEstado.text      = s"⚠ $razon"
                btnConectar.disable = false
            )

      contenedor.children = List(titulo, campoIp, campoPuerto, btnConectar, txtEstado, btnVolver)
      content = contenedor

  // =========================================================================
  //  ESCENA: CONFIGURACIÓN DE JUGADORES (MODO LOCAL — hotseat, un solo PC)
  //  El único var local permitido aquí es el contador de la UI de JavaFX;
  //  lo encapsulamos en una propiedad observable de ScalaFX para que
  //  los botones reaccionen sin mutar estado externo.
  // =========================================================================
  def crearEscenaConfiguracion(): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(25, 25, 25)

      val titulo = new Text("¿Cuántos jugadores?"):
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 32)

      // Propiedad observable: el número de jugadores vive aquí,
      // aislado dentro de esta escena, y los botones la actualizan
      // a través de la propiedad (patrón reactivo de JavaFX).
      val numJugadoresProp = new javafx.beans.property.SimpleIntegerProperty(3)

      val camposNombre: List[TextField] = (1 to 10).map { i =>
        new TextField:
          promptText = s"Nombre Jugador $i"
          text       = s"Jugador $i"
          prefWidth  = 280
          style      = "-fx-font-size: 14px;"
          visible    = i <= 3
          managed    = i <= 3
      }.toList

      // Función pura: dado un número, actualiza visibilidad de campos
      def actualizarCampos(n: Int): Unit =
        camposNombre.zipWithIndex.foreach { case (campo, i) =>
          campo.visible = i < n
          campo.managed = i < n
        }
        txtNum.text = s"$n jugadores"

      val txtNum = new Text(s"${numJugadoresProp.get} jugadores"):
        fill = White
        font = Font.font("Arial", 18)

      val btnMenos = new Button("−"):
        prefWidth  = 50
        prefHeight = 40
        style      = estiloBoton("#555555")
        onAction   = () =>
          val n = numJugadoresProp.get
          if n > 3 then
            numJugadoresProp.set(n - 1)
            actualizarCampos(n - 1)

      val btnMas = new Button("+"):
        prefWidth  = 50
        prefHeight = 40
        style      = estiloBoton("#555555")
        onAction   = () =>
          val n = numJugadoresProp.get
          if n < 10 then
            numJugadoresProp.set(n + 1)
            actualizarCampos(n + 1)

      val filaBotones = new HBox(20):
        alignment = Pos.Center
        children  = List(btnMenos, txtNum, btnMas)

      val txtInfo = new Text("Los roles (Buscador / Saboteador) se asignan en secreto al iniciar."):
        fill = LightGray
        font = Font.font("Arial", 13)

      val btnIniciar = new Button("INICIAR PARTIDA"):
        prefWidth  = 280
        prefHeight = 55
        style      = estiloBoton("#2e7d32")
        onAction   = () =>
          val n      = numJugadoresProp.get
          val nombres = camposNombre.take(n).map(_.text.value.trim)
            .map(n => if n.isEmpty then "Jugador" else n)
          val estadoInicial = EstadoUI(juego = FabricaJuego.crearJuego(nombres))
          stage.scene = crearEscenaJuego(estadoInicial)

      val btnVolver = new Button("VOLVER"):
        prefWidth  = 280
        prefHeight = 45
        style      = estiloBoton("#555555")
        onAction   = () => stage.scene = crearEscenaMenu()

      val layout = new VBox(18):
        alignment = Pos.Center
        padding   = Insets(30)
        style     = "-fx-background-color: #191919;"
        children  = List(titulo, filaBotones) ++ camposNombre ++ List(txtInfo, btnIniciar, btnVolver)

      layout.prefWidth  = anchoVentana
      layout.prefHeight = altoVentana
      content           = layout

  // =========================================================================
  //  ESCENA: JUEGO
  //  Recibe un EstadoUI inmutable. Cada acción construye un nuevo EstadoUI
  //  y llama a renderizar(nuevoEstado) — nunca muta nada.
  // =========================================================================
  def crearEscenaJuego(estadoInicial: EstadoUI, red: Option[ContextoRed] = None): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(30, 30, 30)
      val contenedor = new Pane()
      contenedor.prefWidth  = anchoVentana
      contenedor.prefHeight = altoVentana

      // Único punto de mutación de esta pantalla: necesario para que los
      // mensajes de red que llegan de forma asíncrona (ESTADO/ERROR del host,
      // cuando SOY un cliente remoto) sepan sobre qué EstadoUI partir.
      var estadoVar: EstadoUI = estadoInicial

      // ── renderizar: único punto de efecto, recibe estado puro ──────────────
      def renderizar(estado: EstadoUI): Unit =
        estadoVar = estado
        contenedor.children.clear()
        estado.juego.estadoPartida match
          case EstadoPartida.GanoBuscadores   =>
            mostrarPantallaFin("¡¡LOS BUSCADORES GANARON!!", "¡Encontraron el oro!", Gold, estado)
          case EstadoPartida.GanoSaboteadores =>
            mostrarPantallaFin("¡¡LOS SABOTEADORES GANARON!!", "El mazo se agotó sin llegar al oro.", Silver, estado)
          case EstadoPartida.EnCurso          =>
            renderizarJuego(estado, renderizar)

      // ── Si soy el HOST, registrar callback para refrescar la UI cuando
      //    un cliente remoto juegue su turno.
      red.foreach { ctx =>
        ctx.servidor.foreach { srv =>
          srv.registrarUIHost(j => renderizar(estadoVar.conJuego(j)))
        }
      }

      // ── Si soy un cliente remoto, los mensajes que llegan del host después
      //    de esta pantalla (ESTADO tras una jugada, ERROR, desconexión) se
      //    redirigen aquí en vez de a la pantalla de "conectando…".
      red.foreach { ctx =>
        ctx.cliente.foreach { cli =>
          cli.alRecibir {
            case EstadoJuegoMsg(j) => renderizar(estadoVar.conJuego(j))
            case ErrorMsg(r)       => renderizar(estadoVar.conError(r))
            case _                 => ()
          }
          cli.alPerderConexion(razon => renderizar(estadoVar.conError(s"⚠ $razon")))
        }
      }

      // ── Pantalla fin de partida ────────────────────────────────────────────
      def mostrarPantallaFin(titFin: String, subtitFin: String,
                              colorTit: javafx.scene.paint.Color,
                              estado: EstadoUI): Unit =
        val txtTit = new Text(titFin):
          fill = colorTit
          font = Font.font("Arial", FontWeight.Bold, 42)

        val txtSub = new Text(subtitFin):
          fill = White
          font = Font.font("Arial", 20)

        val txtRoles = estado.juego.listaJugadores.map { j =>
          new Text(s"${j.nombre}: ${j.rol}"):
            fill = if j.rol == Rol.BUSCADOR then LightGreen else Crimson
            font = Font.font("Arial", 16)
        }

        val btnReiniciar = new Button("JUGAR DE NUEVO"):
          prefWidth  = 250
          prefHeight = 50
          style      = estiloBoton("#2e7d32")
          onAction   = () => stage.scene = crearEscenaSeleccionModo()

        val btnSalir = new Button("SALIR"):
          prefWidth  = 250
          prefHeight = 50
          style      = estiloBoton("#c62828")
          onAction   = () => sys.exit(0)

        val layout = new VBox(18):
          alignment = Pos.Center
          style     = "-fx-background-color: #191919;"
          children  = List(txtTit, txtSub) ++ txtRoles ++ List(btnReiniciar, btnSalir)

        layout.prefWidth  = anchoVentana
        layout.prefHeight = altoVentana
        layout.layoutX    = 0
        layout.layoutY    = 0
        contenedor.children.add(layout)

      // ── Renderizado principal del juego ────────────────────────────────────
      def renderizarJuego(estado: EstadoUI, render: EstadoUI => Unit): Unit =
        val jugadorTurno = estado.juego.jugadorActual
        // En red, esta pantalla siempre muestra MI mano/rol, no la de quien
        // tenga el turno (cada jugador tiene su propia pantalla/proceso).
        // En local (hotseat) "jugador" y "jugadorTurno" son lo mismo.
        val miIdOpt   = red.map(_.miId)
        val jugador   = miIdOpt.flatMap(id => estado.juego.listaJugadores.find(_.id == id)).getOrElse(jugadorTurno)
        val esMiTurno = miIdOpt.forall(_ == jugadorTurno.id)

        // ── Única vía para ejecutar una acción de juego ───────────────────
        // Local: se aplica directamente. Host: se aplica y se retransmite a
        // los clientes. Cliente: se manda al host y se espera el ESTADO/ERROR
        // (llegará de forma asíncrona vía el manejador registrado arriba).
        def ejecutarAccion(msg: MensajeRed): Unit =
          red match
            case Some(ctx) if estado.juego.jugadorActual.id != ctx.miId =>
              render(estado.conError("No es tu turno."))
            case None =>
              aplicarAccionAJuego(estado.juego, msg, estado.juego.jugadorActual.id) match
                case ResultadoAccion.Exito(nj, _) => render(estado.conJuego(nj))
                case ResultadoAccion.Error(r)      => render(estado.conError(r))
            case Some(ctx) =>
              ctx.servidor match
                case Some(srv) =>
                  srv.procesarAccionHost(msg, ctx.miId) match
                    case ResultadoAccion.Exito(nj, _) => render(estado.conJuego(nj))
                    case ResultadoAccion.Error(r)      => render(estado.conError(r))
                case None =>
                  ctx.cliente.foreach(_.enviarAccion(msg))
                  // No renderizamos aún: el ESTADO (o ERROR) llega después por red.

        // Barra superior
        val fondoBarra = new Rectangle:
          x = 0; y = 0; width = anchoVentana; height = offsetY
          fill = rgb(20, 20, 20)

        val txtTurno = new Text:
          x = 10; y = 38
          text = s"Turno ${estado.juego.turnoActual}  |  Mazo: ${estado.juego.mazo.cartasRobo.size}  |  Juega: ${jugadorTurno.nombre}"
          fill = LightGreen
          font = Font.font("Arial", FontWeight.Bold, 14)

        val txtRol = new Text:
          x = anchoVentana - 180; y = 25
          text = s"ROL: ${jugador.rol}"
          fill = if jugador.rol == Rol.BUSCADOR then LightGreen else Crimson
          font = Font.font("Arial", FontWeight.Bold, 13)

        val txtHerramientas = new Text:
          x = anchoVentana - 180; y = 45
          text = if jugador.herramientasRotas.isEmpty then "✓ Sin bloqueos"
                 else s"✗ Roto: ${jugador.herramientasRotas.mkString(", ")}"
          fill = if jugador.herramientasRotas.isEmpty then LightGreen else Red
          font = Font.font("Arial", 12)

        contenedor.children.addAll(fondoBarra, txtTurno, txtRol, txtHerramientas)

        // Mensajes
        if estado.juego.mensajeAlerta.nonEmpty then
          val txtAlerta = new Text:
            x = 10; y = offsetY + 18
            text = s"► ${estado.juego.mensajeAlerta}"
            fill = Yellow
            font = Font.font("Arial", FontWeight.Bold, 12)
          contenedor.children.add(txtAlerta)

        if estado.mensajeError.nonEmpty then
          val txtErr = new Text:
            x = 10; y = offsetY + 33
            text = s"⚠ ${estado.mensajeError}"
            fill = OrangeRed
            font = Font.font("Arial", 12)
          contenedor.children.add(txtErr)

        if red.isDefined && !esMiTurno then
          val txtEspera = new Text:
            x = anchoVentana - 270; y = offsetY + 18
            text = s"⏳ Esperando turno de ${jugadorTurno.nombre}…"
            fill = Orange
            font = Font.font("Arial", FontWeight.Bold, 12)
          contenedor.children.add(txtEspera)

        // Tablero
        val inicioTableroY = offsetY + 45

        val bordeTablero = new Rectangle:
          x = Tablero.limiteIzquierdo * anchoCarta
          y = Tablero.limiteArriba * altoCarta + inicioTableroY
          width  = (Tablero.limiteDerecho - Tablero.limiteIzquierdo + 1) * anchoCarta
          height = (Tablero.limiteAbajo   - Tablero.limiteArriba    + 1) * altoCarta
          fill        = Transparent
          stroke      = rgb(80, 80, 80)
          strokeWidth = 1
        contenedor.children.add(bordeTablero)

        estado.juego.tablero.cuadricula.foreach { case (pos, carta) =>
          val px = pos.x * anchoCarta
          val py = pos.y * altoCarta + inicioTableroY
          val iv = crearImageView(carta, anchoCarta, altoCarta, estado.cartasVolteadas)
          iv.layoutX = px
          iv.layoutY = py
          contenedor.children.add(iv)
        }

        // Separador y panel inferior
        val sepY = altoVentana - altoPanelInf
        val sep = new Rectangle:
          x = 0; y = sepY; width = anchoVentana; height = 3
          fill = DimGray
        contenedor.children.add(sep)

        val fondoPanel = new Rectangle:
          x = 0; y = sepY + 3; width = anchoVentana; height = altoPanelInf
          fill = rgb(20, 20, 20)
        contenedor.children.add(fondoPanel)

        // Instrucción contextual — función pura
        val instruccion = estado.cartaSeleccionada match
          case None                                                         => "Selecciona una carta de tu mano"
          case Some(_: CartaTunel)                                          => "Haz clic en el tablero para colocar el túnel  |  [ESC] para cancelar"
          case Some(a: CartaAccion) if a.tipoEfecto == TipoAccion.MAPA     => "Haz clic sobre una carta META para inspeccionarla  |  [ESC] cancelar"
          case Some(a: CartaAccion) if a.tipoEfecto == TipoAccion.DERRUMBE => "Haz clic en un túnel del tablero para derrumbarlo  |  [ESC] cancelar"
          case Some(_: CartaAccion) =>
            if estado.jugadorObjetivoSel.isDefined then "Carta lista — haz clic en cualquier lugar del tablero para confirmar"
            else "Selecciona el jugador objetivo con los botones de abajo  |  [ESC] cancelar"

        val txtInstr = new Text:
          x = 10; y = sepY + 20
          text = instruccion
          fill = LightGray
          font = Font.font("Arial", 12)
        contenedor.children.add(txtInstr)

        // Mano del jugador
        val manoY   = sepY + 32
        val espacio = anchoCartaMano + 13

        // Función pura: ¿es volteable esta carta?
        def esVolteable(c: Carta): Boolean = c match
          case t: CartaTunel => !t.esMeta &&
            t.nombre != "Cruce" && t.nombre != "Túnel Recto H" &&
            t.nombre != "Túnel Recto V" && t.nombre != "SC Cruce" &&
            t.nombre != "SC Recto H"    && t.nombre != "SC Recto V"
          case _ => false

        jugador.mano.zipWithIndex.foreach { case (carta, i) =>
          val cx      = 10 + i * espacio
          val cy      = manoY
          val esSelec  = estado.cartaSeleccionada.exists(_.id == carta.id)
          val esVuelta = estado.cartasVolteadas.getOrElse(carta.id, false)

          if esVuelta then
            val bordeNaranja = new Rectangle:
              x = cx - 5; y = cy - 5
              width = anchoCartaMano + 10; height = altoCartaMano + 10
              fill = Transparent; stroke = Orange; strokeWidth = 2
              arcWidth = 10; arcHeight = 10
            contenedor.children.add(bordeNaranja)

          if esSelec then
            val borde = new Rectangle:
              x = cx - 3; y = cy - 3
              width = anchoCartaMano + 6; height = altoCartaMano + 6
              fill = Transparent; stroke = Yellow; strokeWidth = 3
              arcWidth = 10; arcHeight = 10
            contenedor.children.add(borde)

          val iv = crearImageView(carta, anchoCartaMano, altoCartaMano, estado.cartasVolteadas)
          iv.layoutX = cx
          iv.layoutY = cy

          iv.onMouseClicked = (e: MouseEvent) =>
            import scalafx.scene.input.MouseButton
            val nuevoEstado =
              if e.button == MouseButton.Secondary && esVolteable(carta) then
                estado.voltearCarta(carta.id)
              else
                estado.seleccionar(carta)
            render(nuevoEstado)

          contenedor.children.add(iv)

          if esVolteable(carta) then
            val hint = new Text:
              x = cx + 4; y = cy + altoCartaMano + 11
              text = if esVuelta then "↕ volteada [clic der]" else "↕ voltear [clic der]"
              fill = if esVuelta then Orange else DimGray
              font = Font.font("Arial", 8)
            contenedor.children.add(hint)
        }

        // Botón descartar
        val btnDescartar = new Button("DESCARTAR"):
          layoutX    = anchoVentana - 150
          layoutY    = manoY + 20
          prefWidth  = 130
          prefHeight = 38
          style      = estiloBoton("#555555")
          disable    = estado.cartaSeleccionada.isEmpty || (red.isDefined && !esMiTurno)
          onAction   = () =>
            estado.cartaSeleccionada match
              case Some(carta) => ejecutarAccion(AccionDescartar(carta.id))
              case None        => render(estado.conError("Selecciona primero una carta."))

        contenedor.children.add(btnDescartar)

        // Panel jugadores objetivo
        val necesitaObjetivo = estado.cartaSeleccionada.exists {
          case a: CartaAccion => a.tipoEfecto match
            case TipoAccion.SABOTAJE(_) | TipoAccion.REPARACION(_) => true
            case _                                                  => false
          case _ => false
        }

        if necesitaObjetivo then
          val txtObjLabel = new Text:
            x = 10; y = manoY + altoCartaMano + 22
            text = "Selecciona jugador objetivo:"
            fill = Yellow
            font = Font.font("Arial", FontWeight.Bold, 12)
          contenedor.children.add(txtObjLabel)

          estado.juego.listaJugadores.zipWithIndex.foreach { case (j, idx) =>
            val esObjetivoSel = estado.jugadorObjetivoSel.contains(j.id)
            val btnObj = new Button(s"${j.nombre}\n${j.herramientasRotas.mkString(",")}"):
              layoutX    = 10 + idx * 140
              layoutY    = manoY + altoCartaMano + 30
              prefWidth  = 130
              prefHeight = 50
              style      = estiloBoton(if esObjetivoSel then "#f57f17" else "#37474f")
              onAction   = () => render(estado.seleccionarObjetivo(j.id))
            contenedor.children.add(btnObj)
          }

        // ── Clics en el tablero ─────────────────────────────────────────────
        contenedor.onMouseClicked = (event: MouseEvent) =>
          val inicioTableroY = offsetY + 45
          val enTablero = event.y > inicioTableroY && event.y < (altoVentana - altoPanelInf)
          if enTablero then
            val posDestino = Posicion(
              event.x.toInt / anchoCarta,
              ((event.y - inicioTableroY) / altoCarta).toInt
            )
            estado.cartaSeleccionada match

              case Some(carta: CartaTunel) =>
                val estaVolteada = estado.cartasVolteadas.getOrElse(carta.id, false)
                ejecutarAccion(AccionColocarTunel(carta.id, posDestino.x, posDestino.y, estaVolteada))

              case Some(carta: CartaAccion) if carta.tipoEfecto == TipoAccion.MAPA =>
                ejecutarAccion(AccionMapa(carta.id, posDestino.x, posDestino.y))

              case Some(carta: CartaAccion) if carta.tipoEfecto == TipoAccion.DERRUMBE =>
                ejecutarAccion(AccionDerrumbe(carta.id, posDestino.x, posDestino.y))

              case Some(carta: CartaAccion) =>
                estado.jugadorObjetivoSel match
                  case None => render(estado.conError("Primero selecciona el jugador objetivo abajo."))
                  case Some(objId) =>
                    carta.tipoEfecto match
                      case TipoAccion.SABOTAJE(_)   => ejecutarAccion(AccionSabotaje(carta.id, objId))
                      case TipoAccion.REPARACION(_) => ejecutarAccion(AccionReparacion(carta.id, objId))
                      case _                        => render(estado.conError("Acción no reconocida."))

              case None =>
                render(estado.conError("Selecciona primero una carta de tu mano."))

        // Tecla ESC
        onKeyPressed = key =>
          if key.getCode.toString == "ESCAPE" then
            render(estado.deseleccionar)

      renderizar(estadoInicial)
      content = contenedor