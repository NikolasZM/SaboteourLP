//> using dep "org.scalafx::scalafx:20.0.0-R31"

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.{Rectangle, Line, Circle}
import scalafx.scene.layout.{Pane, VBox, HBox}
import scalafx.scene.text.{Text, Font, FontWeight}
import scalafx.scene.control.{Button, TextField, Label}
import scalafx.scene.input.MouseEvent
import scalafx.geometry.{Pos, Insets}
import scalafx.Includes.*

object InterfazJuego extends JFXApp3:

  // ── Estado mutable de la UI (mínimo indispensable) ────────────────────────
  var estadoJuego: Juego                  = _
  var cartaSeleccionada: Option[Carta]    = None
  var mensajeError: String                = ""
  // Para cartas de acción que apuntan a un jugador (sabotaje/reparación)
  var jugadorObjetivoSeleccionado: Option[Int] = None

  // ── Dimensiones del tablero ───────────────────────────────────────────────
  val anchoCarta   = 60
  val altoCarta    = 80
  val offsetY      = 55   // espacio para la barra de info superior
  val altoPanelInf = 190  // altura del panel inferior (mano + controles)
  val anchoVentana = 900
  val altoVentana  = 780

  // ── Estilos reutilizables ─────────────────────────────────────────────────
  def estiloBoton(color: String) =
    s"-fx-font-size: 14px; -fx-background-color: $color; -fx-text-fill: white; -fx-font-weight: bold; -fx-cursor: hand; -fx-background-radius: 6;"

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

      // Campos de nombre para hasta 10 jugadores (mostramos 3 por defecto)
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

      // Botones - / +
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
          estadoJuego       = FabricaJuego.crearJuego(nombres)
          cartaSeleccionada = None
          mensajeError      = ""
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

      // ── Renderizado principal ───────────────────────────────────────────
      def renderizar(): Unit =
        contenedor.children.clear()

        // Verificar fin de partida
        estadoJuego.estadoPartida match
          case EstadoPartida.GanoBuscadores  => mostrarPantallaFin("¡¡LOS BUSCADORES GANARON!!", "¡Encontraron el oro!", Gold)
          case EstadoPartida.GanoSaboteadores => mostrarPantallaFin("¡¡LOS SABOTEADORES GANARON!!", "El mazo se agotó sin llegar al oro.", Silver)
          case EstadoPartida.EnCurso         => renderizarJuego()

      def mostrarPantallaFin(titFin: String, subtitFin: String, colorTit: javafx.scene.paint.Color): Unit =
        contenedor.children.clear()

        val txtTit = new Text(titFin):
          fill = colorTit
          font = Font.font("Arial", FontWeight.Bold, 42)

        val txtSub = new Text(subtitFin):
          fill = White
          font = Font.font("Arial", 20)

        // Mostrar roles de cada jugador al final
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

        // Centrar el VBox dentro del Pane
        layout.layoutX = 0
        layout.layoutY = 0
        contenedor.children.add(layout)

      def renderizarJuego(): Unit =
        val jugador = estadoJuego.jugadorActual

        // ── Barra superior de info ────────────────────────────────────────
        val fondoBarra = new Rectangle:
          x = 0; y = 0; width = anchoVentana; height = offsetY
          fill = rgb(20, 20, 20)

        val txtTurno = new Text:
          x = 10; y = 38
          text = s"Turno ${estadoJuego.turnoActual}  |  Mazo: ${estadoJuego.mazo.cartasRobo.size}  |  Juega: ${jugador.nombre}"
          fill = LightGreen
          font = Font.font("Arial", FontWeight.Bold, 14)

        val txtRol = new Text:
          x = anchoVentana - 160; y = 25
          text = s"ROL: ${jugador.rol}"
          fill = if jugador.rol == Rol.BUSCADOR then LightGreen else Crimson
          font = Font.font("Arial", FontWeight.Bold, 13)

        // Herramientas rotas del jugador actual
        val txtHerramientas = new Text:
          x = anchoVentana - 160; y = 45
          text = if jugador.herramientasRotas.isEmpty then "✓ Sin bloqueos"
                 else s"✗ Roto: ${jugador.herramientasRotas.mkString(", ")}"
          fill = if jugador.herramientasRotas.isEmpty then LightGreen else Red
          font = Font.font("Arial", 12)

        contenedor.children.addAll(fondoBarra, txtTurno, txtRol, txtHerramientas)

        // ── Mensaje de alerta / error ─────────────────────────────────────
        if estadoJuego.mensajeAlerta.nonEmpty then
          val txtAlerta = new Text:
            x = 10; y = offsetY + 18
            text = s"► ${estadoJuego.mensajeAlerta}"
            fill = Yellow
            font = Font.font("Arial", FontWeight.Bold, 12)
          contenedor.children.add(txtAlerta)

        if mensajeError.nonEmpty then
          val txtErr = new Text:
            x = 10; y = offsetY + 32
            text = s"⚠ $mensajeError"
            fill = OrangeRed
            font = Font.font("Arial", 12)
          contenedor.children.add(txtErr)

        // ── Tablero ───────────────────────────────────────────────────────
        val inicioTableroY = offsetY + 40

        estadoJuego.tablero.cuadricula.foreach { case (pos, carta) =>
          val px = pos.x * anchoCarta
          val py = pos.y * altoCarta + inicioTableroY

          // Color de fondo de la carta
          val colorFondo =
            if pos == estadoJuego.tablero.posicionInicio then LightBlue
            else if carta.esMeta then
              if carta.estaOculta then rgb(80, 30, 30)
              else if carta.esOro  then Gold
              else                      DimGray
            else SaddleBrown

          val rect = new Rectangle:
            x = px; y = py; width = anchoCarta; height = altoCarta
            fill   = colorFondo
            stroke = Black

          contenedor.children.add(rect)

          // Dibujar caminos si la carta no está oculta
          if !carta.estaOculta then
            val cx = px + anchoCarta / 2.0
            val cy = py + altoCarta  / 2.0

            def linea(x1: Double, y1: Double, x2: Double, y2: Double) =
              new Line:
                startX = x1; startY = y1; endX = x2; endY = y2
                stroke      = if carta.esCallejonSinSalida then rgb(180, 50, 50) else LightGray
                strokeWidth = 6

            if carta.arriba    then contenedor.children.add(linea(cx, cy, cx, py))
            if carta.abajo     then contenedor.children.add(linea(cx, cy, cx, py + altoCarta))
            if carta.izquierda then contenedor.children.add(linea(cx, cy, px, cy))
            if carta.derecha   then contenedor.children.add(linea(cx, cy, px + anchoCarta, cy))

            if carta.esCallejonSinSalida then
              val xMark = new Text:
                x = cx - 5; y = cy + 5
                text = "✖"; fill = Red; font = Font.font("Arial", FontWeight.Bold, 14)
              contenedor.children.add(xMark)

          // Etiqueta de la carta
          val etiqueta =
            if pos == estadoJuego.tablero.posicionInicio then "Inicio"
            else if carta.esMeta then (if carta.estaOculta then "?" else if carta.esOro then "ORO" else "X")
            else ""

          if etiqueta.nonEmpty then
            val txt = new Text:
              x = px + 4; y = py + 14
              text = etiqueta
              fill = if pos == estadoJuego.tablero.posicionInicio then Black else White
              font = Font.font("Arial", FontWeight.Bold, 11)
            contenedor.children.add(txt)
        }

        // ── Separador ─────────────────────────────────────────────────────
        val sepY = altoVentana - altoPanelInf
        val sep = new Rectangle:
          x = 0; y = sepY; width = anchoVentana; height = 3
          fill = DimGray
        contenedor.children.add(sep)

        // ── Panel inferior: instrucciones + mano ──────────────────────────
        val fondoPanel = new Rectangle:
          x = 0; y = sepY + 3; width = anchoVentana; height = altoPanelInf
          fill = rgb(20, 20, 20)
        contenedor.children.add(fondoPanel)

        // Instrucción contextual según carta seleccionada
        val instruccion = cartaSeleccionada match
          case None                                                      => "Selecciona una carta de tu mano"
          case Some(_: CartaTunel)                                       => "Haz clic en el tablero para colocar el túnel  |  [ESC] para cancelar"
          case Some(a: CartaAccion) if a.tipoEfecto == TipoAccion.MAPA  => "Haz clic sobre una carta META para inspeccionarla  |  [ESC] cancelar"
          case Some(a: CartaAccion) if a.tipoEfecto == TipoAccion.DERRUMBE => "Haz clic en un túnel del tablero para derrumbarlo  |  [ESC] cancelar"
          case Some(_: CartaAccion)                                      =>
            if jugadorObjetivoSeleccionado.isDefined then "Carta lista — haz clic en cualquier lugar del tablero para confirmar"
            else "Selecciona el jugador objetivo con los botones de abajo  |  [ESC] cancelar"

        val txtInstr = new Text:
          x = 10; y = sepY + 20
          text = instruccion
          fill = LightGray
          font = Font.font("Arial", 12)
        contenedor.children.add(txtInstr)

        // ── Mano del jugador ──────────────────────────────────────────────
        val manoY = sepY + 30
        jugador.mano.zipWithIndex.foreach { case (carta, i) =>
          val cx = 10 + i * 132
          val cy = manoY

          val esSelec = cartaSeleccionada.exists(_.id == carta.id)

          val colorCarta = carta match
            case t: CartaTunel  => if t.esCallejonSinSalida then DarkGoldenrod else rgb(40, 100, 40)
            case a: CartaAccion => a.tipoEfecto match
              case TipoAccion.SABOTAJE(_)   => rgb(140, 20, 20)
              case TipoAccion.REPARACION(_) => rgb(20, 80, 140)
              case TipoAccion.MAPA          => rgb(80, 20, 140)
              case TipoAccion.DERRUMBE      => rgb(120, 60, 20)

          val rectCarta = new Rectangle:
            x = cx; y = cy; width = 125; height = 90
            fill        = colorCarta
            stroke      = if esSelec then Yellow else rgb(80, 80, 80)
            strokeWidth = if esSelec then 3 else 1
            arcWidth    = 8; arcHeight = 8

          rectCarta.onMouseClicked = (_: MouseEvent) =>
            cartaSeleccionada           = Some(carta)
            jugadorObjetivoSeleccionado = None
            mensajeError                = ""
            renderizar()

          val txtNombre = new Text:
            x = cx + 6; y = cy + 30
            text = carta.nombre
            fill = White
            font = Font.font("Arial", FontWeight.Bold, 10)

          // Mini-descripción de la carta
          val descripcion = carta match
            case t: CartaTunel  => if t.esCallejonSinSalida then "Bloquea caminos" else "Túnel"
            case a: CartaAccion => a.tipoEfecto match
              case TipoAccion.SABOTAJE(h)    => s"Rompe $h"
              case TipoAccion.REPARACION(hs) => s"Repara ${hs.mkString("/")}"
              case TipoAccion.MAPA           => "Ver meta oculta"
              case TipoAccion.DERRUMBE       => "Elimina túnel"

          val txtDesc = new Text:
            x = cx + 6; y = cy + 50
            text = descripcion
            fill = LightGray
            font = Font.font("Arial", 9)

          contenedor.children.addAll(rectCarta, txtNombre, txtDesc)
        }

        // ── Botón descartar ───────────────────────────────────────────────
        val btnDescartar = new Button("DESCARTAR"):
          layoutX = anchoVentana - 140
          layoutY = manoY + 10
          prefWidth  = 120
          prefHeight = 35
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

        // ── Panel de jugadores (para sabotaje/reparación) ─────────────────
        val necesitaObjetivo = cartaSeleccionada.exists {
          case a: CartaAccion => a.tipoEfecto match
            case TipoAccion.SABOTAJE(_) | TipoAccion.REPARACION(_) => true
            case _                                                  => false
          case _ => false
        }

        if necesitaObjetivo then
          val txtObjLabel = new Text:
            x = 10; y = manoY + 105
            text = "Selecciona jugador objetivo:"
            fill = Yellow
            font = Font.font("Arial", FontWeight.Bold, 12)
          contenedor.children.add(txtObjLabel)

          estadoJuego.listaJugadores.zipWithIndex.foreach { case (j, idx) =>
            val esObjetivoSel = jugadorObjetivoSeleccionado.contains(j.id)
            val btnObj = new Button(s"${j.nombre}\n${j.herramientasRotas.mkString(",")}"):
              layoutX    = 10 + idx * 140
              layoutY    = manoY + 115
              prefWidth  = 130
              prefHeight = 50
              style      = estiloBoton(if esObjetivoSel then "#f57f17" else "#37474f")
              onAction   = () =>
                jugadorObjetivoSeleccionado = Some(j.id)
                renderizar()
            contenedor.children.add(btnObj)
          }

      // ── Manejo de clics en el tablero ───────────────────────────────────
      contenedor.onMouseClicked = (event: MouseEvent) =>
        val inicioTableroY = offsetY + 40
        val enTablero      = event.y > inicioTableroY && event.y < (altoVentana - altoPanelInf)

        if enTablero then
          val logicoX   = event.x.toInt / anchoCarta
          val logicoY   = ((event.y - inicioTableroY) / altoCarta).toInt
          val posDestino = Posicion(logicoX, logicoY)

          cartaSeleccionada match

            // Colocar túnel
            case Some(carta: CartaTunel) =>
              estadoJuego.colocarTunel(carta.id, posDestino) match
                case ResultadoAccion.Exito(nuevoJuego, _) =>
                  estadoJuego       = nuevoJuego
                  cartaSeleccionada = None
                  mensajeError      = ""
                case ResultadoAccion.Error(razon) =>
                  mensajeError = razon
              renderizar()

            // Usar lupa (mapa)
            case Some(carta: CartaAccion) if carta.tipoEfecto == TipoAccion.MAPA =>
              estadoJuego.usarMapa(carta.id, posDestino) match
                case ResultadoAccion.Exito(nuevoJuego, _) =>
                  estadoJuego       = nuevoJuego
                  cartaSeleccionada = None
                  mensajeError      = ""
                case ResultadoAccion.Error(razon) =>
                  mensajeError = razon
              renderizar()

            // Derrumbe
            case Some(carta: CartaAccion) if carta.tipoEfecto == TipoAccion.DERRUMBE =>
              estadoJuego.aplicarDerrumbe(carta.id, posDestino) match
                case ResultadoAccion.Exito(nuevoJuego, _) =>
                  estadoJuego       = nuevoJuego
                  cartaSeleccionada = None
                  mensajeError      = ""
                case ResultadoAccion.Error(razon) =>
                  mensajeError = razon
              renderizar()

            // Sabotaje/Reparación: confirmar con clic en tablero si ya hay objetivo
            case Some(carta: CartaAccion) =>
              jugadorObjetivoSeleccionado match
                case None => mensajeError = "Primero selecciona el jugador objetivo abajo."
                case Some(objId) =>
                  val resultado = carta.tipoEfecto match
                    case TipoAccion.SABOTAJE(_)   => estadoJuego.aplicarSabotaje(carta.id, objId)
                    case TipoAccion.REPARACION(_)  => estadoJuego.aplicarReparacion(carta.id, objId)
                    case _                         => ResultadoAccion.Error("Acción no reconocida.")
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

      // ── Tecla ESC para cancelar selección ──────────────────────────────
      onKeyPressed = key =>
        if key.getCode.toString == "ESCAPE" then
          cartaSeleccionada           = None
          jugadorObjetivoSeleccionado = None
          mensajeError                = ""
          renderizar()

      renderizar()
      content = contenedor