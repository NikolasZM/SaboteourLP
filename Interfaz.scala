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

object InterfazJuego extends JFXApp3:

  // ── Estado mutable de la UI ───────────────────────────────────────────────
  var estadoJuego: Juego                       = _
  var cartaSeleccionada: Option[Carta]         = None
  var mensajeError: String                     = ""
  var jugadorObjetivoSeleccionado: Option[Int] = None
  var cartasVolteadas: Map[Int, Boolean]       = Map.empty

  // ── Dimensiones ───────────────────────────────────────────────────────────
  // Las imágenes son 254×169 → relación ~1.5:1 (landscape)
  // Ajusta anchoCarta/altoCarta si quieres cambiar el tamaño en el tablero
  val anchoCarta     = 90    // tablero: ancho de cada celda
  val altoCarta      = 60    // tablero: alto de cada celda
  val anchoCartaMano = 127   // mano: misma relación 1.5:1
  val altoCartaMano  = 85
  val offsetY        = 55    // altura barra info superior
  val altoPanelInf   = 210   // altura panel inferior
  val anchoVentana   = 1100
  val altoVentana    = 740

  // ── Estilos reutilizables ─────────────────────────────────────────────────
  def estiloBoton(color: String) =
    s"-fx-font-size: 14px; -fx-background-color: $color; -fx-text-fill: white; -fx-font-weight: bold; -fx-cursor: hand; -fx-background-radius: 6;"

  // =========================================================================
  //  MAPEO CARTA → ARCHIVO DE IMAGEN
  //  Pon tus imágenes en la carpeta  images/  junto a los .scala
  // =========================================================================
  def nombreImagen(carta: Carta): String = carta match

    // ── Cartas de túnel ──────────────────────────────────────────────────────
    case t: CartaTunel if t.estaOculta         => "Carro.png"
    case t: CartaTunel if t.esMeta && t.esOro  => "Carta_Oro.png"
    case t: CartaTunel if t.esMeta             => "Carta_Carbon.png"
    case t: CartaTunel => t.nombre match
      case "Inicio"                      => "Carta_Escalera.png"
      // ── Con conexión ──────────────────────────────────────────────────────
      case "Cruce"                       => "Carta_Camino2.png"
      case "Túnel Recto H"               => "Carta_Camino1.png"
      case "Túnel Recto V"               => "Carta_Camino7.png"
      case "Cruce en T (sin arriba)"     => "Carta_Camino3.png"  // girado → sin abajo
      case "Cruce en T (sin derecha)"    => "Carta_Camino5.png"  // girado → sin izquierda
      case "Curva (arriba-izquierda)"    => "Carta_Camino4.png"  // girado → abajo-derecha
      case "Curva (abajo-izquierda)"     => "Carta_Camino6.png"  // girado → arriba-derecha
      case "Callejón (solo izquierda)"   => "SinCamino7.png"     // girado → solo derecha
      case "Callejón (solo abajo)"       => "SinCamino9.png"     // girado → solo arriba
      // ── Sin Camino (SC): misma imagen, lógica bloqueada ───────────────────
      case "SC Cruce"                    => "SinCamino2.png"
      case "SC Recto H"                  => "SinCamino1.png"
      case "SC Recto V"                  => "SinCamino8.png"
      case "SC Cruce en T (sin arriba)"  => "SinCamino3.png"  // girado → sin abajo
      case "SC Cruce en T (sin der)"     => "SinCamino5.png"  // girado → sin izquierda
      case "SC Curva (arriba-izq)"       => "SinCamino4.png"  // girado → abajo-derecha
      case "SC Curva (abajo-izq)"        => "SinCamino6.png"  // girado → arriba-derecha
      case _                             => "reverso.png"

    // ── Cartas de acción ─────────────────────────────────────────────────────
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

  // ── Crea un ImageView escalado, rotado 180° si la carta está volteada ────
  // Se combina el estado de la UI (cartasVolteadas, para la mano) con el
  // campo imagenVolteada de la propia CartaTunel (para cartas ya en el tablero).
  def crearImageView(carta: Carta, ancho: Double, alto: Double): ImageView =
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
        onAction   = () => stage.scene = crearEscenaConfiguracion()

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
  //  ESCENA: CONFIGURACIÓN DE JUGADORES
  // =========================================================================
  def crearEscenaConfiguracion(): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(25, 25, 25)

      val titulo = new Text("¿Cuántos jugadores?"):
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 32)

      var numJugadores = 3

      val camposNombre: List[TextField] = (1 to 10).map { i =>
        new TextField:
          promptText = s"Nombre Jugador $i"
          text       = s"Jugador $i"
          prefWidth  = 280
          style      = "-fx-font-size: 14px;"
          visible    = i <= 3
          managed    = i <= 3
      }.toList

      val boxCampos = new VBox(8):
        alignment = Pos.Center
        children  = camposNombre

      val btnMenos = new Button("−"):
        prefWidth  = 50
        prefHeight = 40
        style      = estiloBoton("#555555")

      val txtNum = new Text(s"$numJugadores jugadores"):
        fill = White
        font = Font.font("Arial", 18)

      val btnMas = new Button("+"):
        prefWidth  = 50
        prefHeight = 40
        style      = estiloBoton("#555555")

      def actualizarCampos(): Unit =
        camposNombre.zipWithIndex.foreach { case (campo, i) =>
          campo.visible = i < numJugadores
          campo.managed = i < numJugadores
        }
        txtNum.text = s"$numJugadores jugadores"

      btnMenos.onAction = () =>
        if numJugadores > 3 then
          numJugadores -= 1
          actualizarCampos()

      btnMas.onAction = () =>
        if numJugadores < 10 then
          numJugadores += 1
          actualizarCampos()

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
          val nombres = camposNombre.take(numJugadores).map(_.text.value.trim)
            .map(n => if n.isEmpty then "Jugador" else n)
          estadoJuego                 = FabricaJuego.crearJuego(nombres)
          cartaSeleccionada           = None
          mensajeError                = ""
          jugadorObjetivoSeleccionado = None
          stage.scene = crearEscenaJuego()

      val btnVolver = new Button("VOLVER"):
        prefWidth  = 280
        prefHeight = 45
        style      = estiloBoton("#555555")
        onAction   = () => stage.scene = crearEscenaMenu()

      val layout = new VBox(18):
        alignment = Pos.Center
        padding   = Insets(30)
        style     = "-fx-background-color: #191919;"
        children  = List(titulo, filaBotones, boxCampos, txtInfo, btnIniciar, btnVolver)

      layout.prefWidth  = anchoVentana
      layout.prefHeight = altoVentana
      content           = layout

  // =========================================================================
  //  ESCENA: JUEGO
  // =========================================================================
  def crearEscenaJuego(): Scene =
    new Scene(anchoVentana, altoVentana):
      fill = rgb(30, 30, 30)
      val contenedor = new Pane()
      contenedor.prefWidth  = anchoVentana
      contenedor.prefHeight = altoVentana

      def renderizar(): Unit =
        contenedor.children.clear()
        estadoJuego.estadoPartida match
          case EstadoPartida.GanoBuscadores   => mostrarPantallaFin("¡¡LOS BUSCADORES GANARON!!", "¡Encontraron el oro!", Gold)
          case EstadoPartida.GanoSaboteadores => mostrarPantallaFin("¡¡LOS SABOTEADORES GANARON!!", "El mazo se agotó sin llegar al oro.", Silver)
          case EstadoPartida.EnCurso          => renderizarJuego()

      def mostrarPantallaFin(titFin: String, subtitFin: String, colorTit: javafx.scene.paint.Color): Unit =
        contenedor.children.clear()

        val txtTit = new Text(titFin):
          fill = colorTit
          font = Font.font("Arial", FontWeight.Bold, 42)

        val txtSub = new Text(subtitFin):
          fill = White
          font = Font.font("Arial", 20)

        val txtRoles = estadoJuego.listaJugadores.map { j =>
          new Text(s"${j.nombre}: ${j.rol}"):
            fill = if j.rol == Rol.BUSCADOR then LightGreen else Crimson
            font = Font.font("Arial", 16)
        }

        val btnReiniciar = new Button("JUGAR DE NUEVO"):
          prefWidth  = 250
          prefHeight = 50
          style      = estiloBoton("#2e7d32")
          onAction   = () => stage.scene = crearEscenaConfiguracion()

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

      def renderizarJuego(): Unit =
        val jugador = estadoJuego.jugadorActual

        // ── Barra superior de info ──────────────────────────────────────────
        val fondoBarra = new Rectangle:
          x = 0; y = 0; width = anchoVentana; height = offsetY
          fill = rgb(20, 20, 20)

        val txtTurno = new Text:
          x = 10; y = 38
          text = s"Turno ${estadoJuego.turnoActual}  |  Mazo: ${estadoJuego.mazo.cartasRobo.size}  |  Juega: ${jugador.nombre}"
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

        // ── Mensajes de alerta / error ──────────────────────────────────────
        if estadoJuego.mensajeAlerta.nonEmpty then
          val txtAlerta = new Text:
            x = 10; y = offsetY + 18
            text = s"► ${estadoJuego.mensajeAlerta}"
            fill = Yellow
            font = Font.font("Arial", FontWeight.Bold, 12)
          contenedor.children.add(txtAlerta)

        if mensajeError.nonEmpty then
          val txtErr = new Text:
            x = 10; y = offsetY + 33
            text = s"⚠ $mensajeError"
            fill = OrangeRed
            font = Font.font("Arial", 12)
          contenedor.children.add(txtErr)

        // ── Tablero ─────────────────────────────────────────────────────────
        val inicioTableroY = offsetY + 45

        // Borde del área jugable
        val bordeTablero = new Rectangle:
          x = Tablero.limiteIzquierdo * anchoCarta
          y = Tablero.limiteArriba * altoCarta + inicioTableroY
          width  = (Tablero.limiteDerecho - Tablero.limiteIzquierdo + 1) * anchoCarta
          height = (Tablero.limiteAbajo   - Tablero.limiteArriba    + 1) * altoCarta
          fill        = Transparent
          stroke      = rgb(80, 80, 80)
          strokeWidth = 1
        contenedor.children.add(bordeTablero)

        // ── Renderizar cada carta del tablero como ImageView ────────────────
        estadoJuego.tablero.cuadricula.foreach { case (pos, carta) =>
          val px = pos.x * anchoCarta
          val py = pos.y * altoCarta + inicioTableroY

          val iv = crearImageView(carta, anchoCarta, altoCarta)
          iv.layoutX = px
          iv.layoutY = py
          contenedor.children.add(iv)
        }

        // ── Separador ───────────────────────────────────────────────────────
        val sepY = altoVentana - altoPanelInf
        val sep = new Rectangle:
          x = 0; y = sepY; width = anchoVentana; height = 3
          fill = DimGray
        contenedor.children.add(sep)

        // ── Panel inferior ──────────────────────────────────────────────────
        val fondoPanel = new Rectangle:
          x = 0; y = sepY + 3; width = anchoVentana; height = altoPanelInf
          fill = rgb(20, 20, 20)
        contenedor.children.add(fondoPanel)

        // Instrucción contextual
        val instruccion = cartaSeleccionada match
          case None                                                         => "Selecciona una carta de tu mano"
          case Some(_: CartaTunel)                                          => "Haz clic en el tablero para colocar el túnel  |  [ESC] para cancelar"
          case Some(a: CartaAccion) if a.tipoEfecto == TipoAccion.MAPA     => "Haz clic sobre una carta META para inspeccionarla  |  [ESC] cancelar"
          case Some(a: CartaAccion) if a.tipoEfecto == TipoAccion.DERRUMBE => "Haz clic en un túnel del tablero para derrumbarlo  |  [ESC] cancelar"
          case Some(_: CartaAccion) =>
            if jugadorObjetivoSeleccionado.isDefined then "Carta lista — haz clic en cualquier lugar del tablero para confirmar"
            else "Selecciona el jugador objetivo con los botones de abajo  |  [ESC] cancelar"

        val txtInstr = new Text:
          x = 10; y = sepY + 20
          text = instruccion
          fill = LightGray
          font = Font.font("Arial", 12)
        contenedor.children.add(txtInstr)

        // ── Mano del jugador: ImageView por cada carta ──────────────────────
        val manoY   = sepY + 32
        val espacio = anchoCartaMano + 13   // 13px de margen entre cartas

        jugador.mano.zipWithIndex.foreach { case (carta, i) =>
          val cx = 10 + i * espacio
          val cy = manoY

          val esSelec  = cartaSeleccionada.exists(_.id == carta.id)
          val esVuelta = cartasVolteadas.getOrElse(carta.id, false)

          // Borde naranja = volteada (se dibuja primero, queda detrás)
          if esVuelta then
            val bordeNaranja = new Rectangle:
              x           = cx - 5; y = cy - 5
              width       = anchoCartaMano + 10
              height      = altoCartaMano  + 10
              fill        = Transparent
              stroke      = Orange
              strokeWidth = 2
              arcWidth    = 10; arcHeight = 10
            contenedor.children.add(bordeNaranja)

          // Borde amarillo = seleccionada
          if esSelec then
            val borde = new Rectangle:
              x           = cx - 3; y = cy - 3
              width       = anchoCartaMano + 6
              height      = altoCartaMano  + 6
              fill        = Transparent
              stroke      = Yellow
              strokeWidth = 3
              arcWidth    = 10; arcHeight = 10
            contenedor.children.add(borde)

          val iv = crearImageView(carta, anchoCartaMano, altoCartaMano)
          iv.layoutX = cx
          iv.layoutY = cy

          iv.onMouseClicked = (e: MouseEvent) =>
            import scalafx.scene.input.MouseButton
            if e.button == MouseButton.Secondary then
              // Clic derecho: voltear cartas de túnel no simétricas (incluye SC)
              carta match
                case t: CartaTunel if !t.esMeta &&
                    t.nombre != "Cruce"      &&
                    t.nombre != "Túnel Recto H" &&
                    t.nombre != "Túnel Recto V" &&
                    t.nombre != "SC Cruce"   &&
                    t.nombre != "SC Recto H" &&
                    t.nombre != "SC Recto V" =>
                  cartasVolteadas = cartasVolteadas + (t.id -> !esVuelta)
                case _ => ()
            else
              // Clic izquierdo: seleccionar (siempre la carta original del mazo)
              cartaSeleccionada           = Some(carta)
              jugadorObjetivoSeleccionado = None
              mensajeError                = ""
            renderizar()

          contenedor.children.add(iv)

          // Hint "↕" debajo de cartas volteables
          carta match
            case t: CartaTunel if !t.esMeta &&
                t.nombre != "Cruce"      &&
                t.nombre != "Túnel Recto H" &&
                t.nombre != "Túnel Recto V" &&
                t.nombre != "SC Cruce"   &&
                t.nombre != "SC Recto H" &&
                t.nombre != "SC Recto V" =>
              val hint = new Text:
                x    = cx + 4; y = cy + altoCartaMano + 11
                text = if esVuelta then "↕ volteada [clic der]" else "↕ voltear [clic der]"
                fill = if esVuelta then Orange else DimGray
                font = Font.font("Arial", 8)
              contenedor.children.add(hint)
            case _ => ()
        }

        // ── Botón descartar ─────────────────────────────────────────────────
        val btnDescartar = new Button("DESCARTAR"):
          layoutX    = anchoVentana - 150
          layoutY    = manoY + 20
          prefWidth  = 130
          prefHeight = 38
          style      = estiloBoton("#555555")
          disable    = cartaSeleccionada.isEmpty
          onAction   = () =>
            cartaSeleccionada match
              case Some(carta) =>
                estadoJuego.descartarCarta(carta.id) match
                  case ResultadoAccion.Exito(nuevoJuego, _) =>
                    estadoJuego       = nuevoJuego
                    cartaSeleccionada = None
                    mensajeError      = ""
                  case ResultadoAccion.Error(razon) =>
                    mensajeError = razon
              case None => mensajeError = "Selecciona primero una carta."
            renderizar()

        contenedor.children.add(btnDescartar)

        // ── Panel jugadores objetivo (sabotaje / reparación) ────────────────
        val necesitaObjetivo = cartaSeleccionada.exists {
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

          estadoJuego.listaJugadores.zipWithIndex.foreach { case (j, idx) =>
            val esObjetivoSel = jugadorObjetivoSeleccionado.contains(j.id)
            val btnObj = new Button(s"${j.nombre}\n${j.herramientasRotas.mkString(",")}"):
              layoutX    = 10 + idx * 140
              layoutY    = manoY + altoCartaMano + 30
              prefWidth  = 130
              prefHeight = 50
              style      = estiloBoton(if esObjetivoSel then "#f57f17" else "#37474f")
              onAction   = () =>
                jugadorObjetivoSeleccionado = Some(j.id)
                renderizar()
            contenedor.children.add(btnObj)
          }

      // ── Manejo de clics en el tablero ────────────────────────────────────
      contenedor.onMouseClicked = (event: MouseEvent) =>
        val inicioTableroY = offsetY + 45
        val enTablero      = event.y > inicioTableroY && event.y < (altoVentana - altoPanelInf)

        if enTablero then
          val logicoX    = event.x.toInt / anchoCarta
          val logicoY    = ((event.y - inicioTableroY) / altoCarta).toInt
          val posDestino = Posicion(logicoX, logicoY)

          cartaSeleccionada match

            case Some(carta: CartaTunel) =>
              val estaVolteada = cartasVolteadas.getOrElse(carta.id, false)
              estadoJuego.colocarTunel(carta.id, posDestino, voltear = estaVolteada) match
                case ResultadoAccion.Exito(nuevoJuego, _) =>
                  estadoJuego       = nuevoJuego
                  cartaSeleccionada = None
                  cartasVolteadas   = Map.empty
                  mensajeError      = ""
                case ResultadoAccion.Error(razon) =>
                  mensajeError = razon
              renderizar()

            case Some(carta: CartaAccion) if carta.tipoEfecto == TipoAccion.MAPA =>
              estadoJuego.usarMapa(carta.id, posDestino) match
                case ResultadoAccion.Exito(nuevoJuego, _) =>
                  estadoJuego       = nuevoJuego
                  cartaSeleccionada = None
                  mensajeError      = ""
                case ResultadoAccion.Error(razon) =>
                  mensajeError = razon
              renderizar()

            case Some(carta: CartaAccion) if carta.tipoEfecto == TipoAccion.DERRUMBE =>
              estadoJuego.aplicarDerrumbe(carta.id, posDestino) match
                case ResultadoAccion.Exito(nuevoJuego, _) =>
                  estadoJuego       = nuevoJuego
                  cartaSeleccionada = None
                  mensajeError      = ""
                case ResultadoAccion.Error(razon) =>
                  mensajeError = razon
              renderizar()

            case Some(carta: CartaAccion) =>
              jugadorObjetivoSeleccionado match
                case None => mensajeError = "Primero selecciona el jugador objetivo abajo."
                case Some(objId) =>
                  val resultado = carta.tipoEfecto match
                    case TipoAccion.SABOTAJE(_)   => estadoJuego.aplicarSabotaje(carta.id, objId)
                    case TipoAccion.REPARACION(_) => estadoJuego.aplicarReparacion(carta.id, objId)
                    case _                        => ResultadoAccion.Error("Acción no reconocida.")
                  resultado match
                    case ResultadoAccion.Exito(nuevoJuego, _) =>
                      estadoJuego                 = nuevoJuego
                      cartaSeleccionada           = None
                      jugadorObjetivoSeleccionado = None
                      mensajeError                = ""
                    case ResultadoAccion.Error(razon) =>
                      mensajeError = razon
              renderizar()

            case None =>
              mensajeError = "Selecciona primero una carta de tu mano."
              renderizar()

      // ── Tecla ESC para cancelar selección ────────────────────────────────
      onKeyPressed = key =>
        if key.getCode.toString == "ESCAPE" then
          cartaSeleccionada           = None
          jugadorObjetivoSeleccionado = None
          mensajeError                = ""
          renderizar()

      renderizar()
      content = contenedor