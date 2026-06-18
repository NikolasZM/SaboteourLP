//> using dep "org.scalafx::scalafx:20.0.0-R31"

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.{Rectangle, Line}
import scalafx.scene.layout.{Pane, VBox}
import scalafx.scene.text.{Text, Font, FontWeight}
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.geometry.Pos
import scalafx.Includes.*
import scala.util.Random

object InterfazJuego extends JFXApp3 {

  var estadoJuego: Juego = _
  var cartaSeleccionada: Option[Carta] = None

  val anchoCarta = 60
  val altoCarta = 85
  val offsetTableroY = 60

  override def start(): Unit = {
    
    //INICIO
    val layoutMenu = new VBox {
      spacing = 30
      alignment = Pos.Center
      prefWidth = 900
      prefHeight = 740
      style = "-fx-background-color: #222222;"

      val txtTitulo = new Text {
        text = "SABOTAJE"
        fill = Gold
        font = Font.font("Arial", FontWeight.Bold, 60)
      }

      val txtSubtitulo = new Text {
        text = "En busca del oro"
        fill = LightGray
        font = Font.font("Arial", 18)
      }

      val btnJugar = new Button("JUGAR") {
        prefWidth = 200
        prefHeight = 50
        style = "-fx-font-size: 18px; -fx-background-color: #2e7d32; -fx-text-fill: white; -fx-font-weight: bold; -fx-cursor: hand;"
        
        onAction = () => {
          inicializarLogicaJuego()
          stage.scene = crearEscenaJuego()
        }
      }

      val btnSalir = new Button("SALIR") {
        prefWidth = 200
        prefHeight = 50
        style = "-fx-font-size: 18px; -fx-background-color: #c62828; -fx-text-fill: white; -fx-font-weight: bold; -fx-cursor: hand;"
        
        onAction = () => {
          sys.exit(0) // Cierra la aplicación por completo
        }
      }

      children = List(txtTitulo, txtSubtitulo, btnJugar, btnSalir)
    }

    val escenaMenu = new Scene(layoutMenu, 900, 740)

    stage = new JFXApp3.PrimaryStage {
      title = "Saboteur - Menú Principal"
      width = 900
      height = 740
      scene = escenaMenu
    }
  }

  def inicializarLogicaJuego(): Unit = {
    val mazoMezclado = GeneradorMazo.crearMazoMezclado()
    val jugador1 = Jugador(1, "Jugador 1", Rol.BUSCADOR, Nil)
    
    val posInicioLogica = Posicion(2, 3)
    val metasLogicas = List(Posicion(10, 1), Posicion(10, 3), Posicion(10, 5))
    
    val metasDistribuidasAsignadas = Random.shuffle(List(true, false, false))
    
    val cartasMetaLogicas = metasLogicas.zip(metasDistribuidasAsignadas).map { case (pos, esOro) =>
      pos -> CartaTunel(
        id = -pos.y,
        nombre = "Destino Oculto",
        arriba = true, abajo = true, izquierda = true, derecha = true,
        esCallejonSinSalida = false,
        esMeta = true,
        estaOculta = true,
        esOro = esOro
      )
    }.toMap

    val cartaInicioLogica = CartaTunel(0, "Inicio", arriba = true, abajo = true, izquierda = true, derecha = true, esCallejonSinSalida = false)
    val cuadriculaInicial = cartasMetaLogicas + (posInicioLogica -> cartaInicioLogica)
    val tableroInicial = Tablero(cuadriculaInicial, posInicioLogica, metasLogicas)
    
    val juegoBase = Juego(List(jugador1), tableroInicial, mazoMezclado, 1)
    estadoJuego = juegoBase.inicializarPartida()
  }

  def crearEscenaJuego(): Scene = {
    new Scene {
      fill = rgb(35, 35, 35)
      val contenedor = new Pane()

      def renderizar(): Unit = {
        contenedor.children.clear()

        val txtInfo = new Text {
          x = 20; y = 30
          text = s"TURNO: ${estadoJuego.turnoActual}  |  Mazo: ${estadoJuego.mazo.cartasRobo.size} cartas"
          fill = LightGreen; font = Font.font("Arial", 15)
        }
        contenedor.children.add(txtInfo)

        if (estadoJuego.mensajeAlerta.nonEmpty) {
          val txtAlerta = new Text {
            x = 420; y = 30
            text = s"🔬 ¡INSPECCIÓN!: ${estadoJuego.mensajeAlerta}"
            fill = Yellow; font = Font.font("Arial", FontWeight.Bold, 14)
          }
          contenedor.children.add(txtAlerta)
        }

        estadoJuego.tablero.cuadricula.foreach { case (pos, carta) =>
          val posX = pos.x * anchoCarta
          val posY = (pos.y * altoCarta) + offsetTableroY

          val colorCarta = if (pos == estadoJuego.tablero.posicionInicio) {
            LightBlue
          } else if (carta.esMeta) {
            if (carta.estaOculta) rgb(90, 40, 40) else (if (carta.esOro) Gold else Silver)
          } else {
            SaddleBrown
          }

          val rect = new Rectangle {
            x = posX; y = posY; width = anchoCarta; height = altoCarta
            fill = colorCarta; stroke = Black
          }
          contenedor.children.add(rect)

          if (!carta.estaOculta) {
            val centroX = posX + (anchoCarta / 2)
            val centroY = posY + (altoCarta / 2)

            def dibujarCamino(x1: Double, y1: Double, x2: Double, y2: Double) = new Line {
              startX = x1; startY = y1; endX = x2; endY = y2
              stroke = LightGray; strokeWidth = 7
            }

            if (carta.arriba)    contenedor.children.add(dibujarCamino(centroX, centroY, centroX, posY))
            if (carta.abajo)     contenedor.children.add(dibujarCamino(centroX, centroY, centroX, posY + altoCarta))
            if (carta.izquierda) contenedor.children.add(dibujarCamino(centroX, centroY, posX, centroY))
            if (carta.derecha)   contenedor.children.add(dibujarCamino(centroX, centroY, posX + anchoCarta, centroY))

            if (carta.esCallejonSinSalida) {
              val xBloqueo = new Text {
                x = centroX - 5; y = centroY + 5; text = "X"; fill = Red; font = Font.font("Arial", 16)
              }
              contenedor.children.add(xBloqueo)
            }
          }

          val txtNombre = new Text {
            x = posX + 5; y = posY + 20
            text = if (pos == estadoJuego.tablero.posicionInicio) "Inicio" else if (carta.esMeta) "Meta" else "Túnel"
            fill = if (pos == estadoJuego.tablero.posicionInicio) Black else White; font = Font.font("Arial", 10)
          }
          contenedor.children.add(txtNombre)
        }

        val lineaDivisoria = new Rectangle {
          x = 0; y = 560; width = 900; height = 4; fill = Gray
        }
        val txtManoInfo = new Text {
          x = 20; y = 585; text = "TU MANO (Selecciona una carta y haz clic en su objetivo en el mapa):"; fill = White; font = Font.font("Arial", 13)
        }
        contenedor.children.addAll(lineaDivisoria, txtManoInfo)

        val jugadorActual = estadoJuego.listaJugadores.head
        jugadorActual.mano.zipWithIndex.foreach { case (carta, indice) =>
          val posX = 30 + (indice * 140)
          val posY = 605
          
          val esSeleccionada = cartaSeleccionada.exists(_.id == carta.id)

          val colorFondo = carta match {
            case t: CartaTunel => if (t.esCallejonSinSalida) DarkGoldenrod else DarkGreen
            case a: CartaAccion => a.tipoEfecto match {
              case TipoAccion.SABOTAJE(_)   => Crimson
              case TipoAccion.REPARACION(_) => DeepSkyBlue
              case TipoAccion.MAPA          => Purple
              case TipoAccion.DERRUMBE      => Chocolate
            }
          }

          val rectCarta = new Rectangle {
            x = posX; y = posY; width = 125; height = 80
            fill = colorFondo; stroke = if (esSeleccionada) Yellow else Black
            strokeWidth = if (esSeleccionada) 3 else 1
          }

          rectCarta.onMouseClicked = (e: MouseEvent) => {
            cartaSeleccionada = Some(carta)
            renderizar()
          }

          val txtCarta = new Text {
            x = posX + 8; y = posY + 45
            text = carta.nombre; fill = White; font = Font.font("Arial", 11)
          }
          contenedor.children.addAll(rectCarta, txtCarta)
        }
      }

      onMouseClicked = (event: MouseEvent) => {
        if (event.y > offsetTableroY && event.y < 550) {
          val logicoX = event.x.toInt / anchoCarta
          val logicoY = (event.y.toInt - offsetTableroY) / altoCarta
          val posDestino = Posicion(logicoX, logicoY)

          cartaSeleccionada match {
            case Some(cartaTunel: CartaTunel) =>
              val esValido = estadoJuego.tablero.validarColocacion(posDestino, cartaTunel)

              if (esValido) {
                val nuevaCuadricula = estadoJuego.tablero.cuadricula + (posDestino -> cartaTunel)
                val nuevoTablero = estadoJuego.tablero.copy(cuadricula = nuevaCuadricula)
                val jugadorActual = estadoJuego.listaJugadores.head

                estadoJuego = estadoJuego.avanzarTurnoYRobar(jugadorActual.id, nuevoTablero, cartaTunel.id)
                cartaSeleccionada = None
                renderizar()
              } else {
                println("Movimiento inválido de túnel.")
              }

            case Some(cartaAccion: CartaAccion) if cartaAccion.tipoEfecto == TipoAccion.MAPA =>
              estadoJuego.tablero.cuadricula.get(posDestino) match {
                case Some(cartaDestino) if cartaDestino.esMeta =>
                  val queEs = if (cartaDestino.esOro) "¡¡ORO!!" else "Carbón."
                  val textoRevelador = s"La Meta en (${posDestino.x}, ${posDestino.y}) contiene: $queEs"
                  val jugadorActual = estadoJuego.listaJugadores.head
                  
                  estadoJuego = estadoJuego.avanzarTurnoYRobar(jugadorActual.id, estadoJuego.tablero, cartaAccion.id, textoRevelador)
                  cartaSeleccionada = None
                  renderizar()
                case _ =>
                  println("La lupa solo se puede usar sobre una de las 3 cartas de Meta.")
              }

            case Some(_: CartaAccion) =>
              println("Acción no existe.")
            case None =>
              println("Selecciona primero una carta.")
          }
        }
      }

      renderizar()
      content = contenedor
    }
  }
}