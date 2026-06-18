//> using dep "org.scalafx::scalafx:20.0.0-R31"

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.layout.Pane
import scalafx.scene.text.Text
import scalafx.scene.text.Font
import scalafx.scene.input.MouseEvent
import scalafx.Includes.*
import scalafx.scene.shape.Line


object InterfazJuego extends JFXApp3 {

  var estadoJuego: Juego = _
  var cartaSeleccionada: Option[Carta] = None

  val anchoCarta = 60
  val altoCarta = 90

  override def start(): Unit = {
    val mazoMezclado = GeneradorMazo.crearMazoMezclado()
    val jugador1 = Jugador(1, "Jugador 1", Rol.BUSCADOR, Nil)
    
    val posInicioLogica = Posicion(2, 3)
    val metasLogicas = List(Posicion(12, 3))
    
    val cartaInicioLogica = CartaTunel(0, "Inicio", arriba = true, abajo = true, izquierda = true, derecha = true, esCallejonSinSalida = false)
    val tableroInicial = Tablero(Map(posInicioLogica -> cartaInicioLogica), posInicioLogica, metasLogicas)
    
    val juegoBase = Juego(List(jugador1), tableroInicial, mazoMezclado, 1) // Empezamos en turno 1
    estadoJuego = juegoBase.inicializarPartida()

    stage = new JFXApp3.PrimaryStage {
      title = "Saboteur - Turnos y Robo Automático"
      width = 950
      height = 680
      
      scene = new Scene {
        fill = rgb(40, 40, 40)
        val contenedor = new Pane()

        def renderizar(): Unit = {
          contenedor.children.clear()

          // --- 1. CONTADOR DE TURNOS E INFO DEL MAZO (SUPERIOR) ---
          val txtInfoSuperior = new Text {
            x = 20
            y = 35
            text = s"TURNO: ${estadoJuego.turnoActual}   |   Cartas restantes en el mazo: ${estadoJuego.mazo.cartasRobo.size}"
            fill = LightGreen
            font = Font.font("Arial", 18)
          }
          contenedor.children.add(txtInfoSuperior)

          // --- 2. DIBUJAR TABLERO DE TÚNELES (Desplazado un poco hacia abajo por el contador) ---
          val offsetTableroY = 50

          estadoJuego.tablero.cuadricula.foreach { case (pos, carta) =>
            val rect = new Rectangle {
              x = pos.x * anchoCarta
              y = (pos.y * altoCarta) + offsetTableroY
              width = anchoCarta
              height = altoCarta
              fill = if (pos == posInicioLogica) LightBlue else SaddleBrown
              stroke = Black
            }
            val txt = new Text {
              x = (pos.x * anchoCarta) + 5
              y = (pos.y * altoCarta) + 45 + offsetTableroY
              text = if (pos == posInicioLogica) "Inicio" else "Túnel"
              fill = if (pos == posInicioLogica) Black else White
            }
            contenedor.children.addAll(rect, txt)
          }

          // Meta visual
          val rectMeta = new Rectangle {
            x = 720; y = 270 + offsetTableroY; width = anchoCarta; height = altoCarta; fill = DarkRed
          }
          val txtMeta = new Text {
            x = 725; y = 320 + offsetTableroY; text = "Destino"; fill = White
          }
          contenedor.children.addAll(rectMeta, txtMeta)

          // --- 3. PANEL DE LA MANO ---
          val lineaDivisoria = new Rectangle {
            x = 0; y = 510; width = 950; height = 5; fill = Gray
          }
          val txtMano = new Text {
            x = 20; y = 540; text = "Tu Mano (Haz clic para seleccionar):"; fill = White
          }
          contenedor.children.addAll(lineaDivisoria, txtMano)

          val jugadorActual = estadoJuego.listaJugadores.head
          jugadorActual.mano.zipWithIndex.foreach { case (carta, indice) =>
            val posX = 50 + (indice * 110)
            val posY = 550
            
            val esSeleccionada = cartaSeleccionada.exists(_.id == carta.id)

            val rectCarta = new Rectangle {
              x = posX
              y = posY
              width = 100
              height = 70
              fill = DarkGreen
              stroke = if (esSeleccionada) Yellow else Black
              strokeWidth = if (esSeleccionada) 3 else 1
            }

            rectCarta.onMouseClicked = (e: MouseEvent) => {
              cartaSeleccionada = Some(carta)
              renderizar()
            }

            val txtCarta = new Text {
              x = posX + 5
              y = posY + 35
              text = carta.nombre.take(15)
              fill = White
            }
            contenedor.children.addAll(rectCarta, txtCarta)
          }
        }

        // --- GESTIÓN DEL CLICK EN EL TABLERO ---
        onMouseClicked = (event: MouseEvent) => {
          val offsetTableroY = 50
          
          // Ajustamos el clic restando el espacio del contador superior
          if (event.y > offsetTableroY && event.y < 500) {
            val logicoX = event.x.toInt / anchoCarta
            val logicoY = (event.y.toInt - offsetTableroY) / altoCarta
            val posDestino = Posicion(logicoX, logicoY)

            cartaSeleccionada match {
              case Some(cartaTunel: CartaTunel) =>
                val esValido = estadoJuego.tablero.validarColocacion(posDestino, cartaTunel)

                if (esValido) {
                  // 1. Colocar la carta en el tablero
                  val nuevaCuadricula = estadoJuego.tablero.cuadricula + (posDestino -> cartaTunel)
                  val nuevoTablero = estadoJuego.tablero.copy(cuadricula = nuevaCuadricula)

                  // 2. Remover de la mano del jugador
                  val jugadorActual = estadoJuego.listaJugadores.head
                  val jugadorModificado = jugadorActual.eliminarCartaDeMano(cartaTunel.id)

                  // Guardamos este estado intermedio temporalmente
                  val estadoTemporal = estadoJuego.copy(
                    tablero = nuevoTablero,
                    listaJugadores = List(jugadorModificado)
                  )

                  // 3. ¡ROBAR AUTOMÁTICAMENTE Y AVANZAR TURNO!
                  estadoJuego = estadoTemporal.avanzarTurnoYRobar(jugadorActual.id)

                  cartaSeleccionada = None
                  println(s"¡Turno completado! Carta puesta en ($logicoX, $logicoY).")
                  renderizar()
                } else {
                  println(s"Jugada inválida en ($logicoX, $logicoY).")
                }

              case Some(_: CartaAccion) =>
                println("Las cartas de acción no van en el tablero de túneles.")
              case None =>
                println("Selecciona una carta primero.")
            }
          }
        }

        renderizar()
        content = contenedor
      }
    }
  }
}