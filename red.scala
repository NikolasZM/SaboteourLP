//> using dep "org.apache.pekko::pekko-actor:1.1.2"
//> using dep "org.apache.pekko::pekko-remote:1.1.2"
//> using dep "com.typesafe:config:1.4.3"

import java.net.InetAddress
import scala.util.Try
import scalafx.application.Platform
import org.apache.pekko.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

// ─────────────────────────────────────────────
//  MENSAJES DE RED (ADT Inmutable y Serializable)
// ─────────────────────────────────────────────
sealed trait MensajeRed extends java.io.Serializable

// Cliente → Host
case class AccionColocarTunel(cartaId: Int, posX: Int, posY: Int, voltear: Boolean) extends MensajeRed
case class AccionSabotaje(cartaId: Int, objetivoId: Int)                             extends MensajeRed
case class AccionReparacion(cartaId: Int, objetivoId: Int)                           extends MensajeRed
case class AccionMapa(cartaId: Int, posX: Int, posY: Int)                            extends MensajeRed
case class AccionDerrumbe(cartaId: Int, posX: Int, posY: Int)                         extends MensajeRed
case class AccionDescartar(cartaId: Int)                                             extends MensajeRed

// Host → Clientes
case class EstadoJuegoMsg(juego: Juego)    extends MensajeRed
case class ErrorMsg(razon: String)         extends MensajeRed
case class BienvenidaMsg(jugadorId: Int)   extends MensajeRed

// ─────────────────────────────────────────────
//  MOTOR DE ACCIONES
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
// ─────────────────────────────────────────────
case class ContextoRed(
  miId:     Int,
  servidor: Option[ServidorJuego] = None,
  cliente:  Option[ClienteJuego]  = None
)

// ─────────────────────────────────────────────
//  SERVIDOR (HOST)
// ─────────────────────────────────────────────
class ServidorJuego(puerto: Int, juegoInicial: Juego):

  private val hostId: Int = juegoInicial.listaJugadores.head.id
  @volatile private var estadoActual: Juego = juegoInicial
  @volatile private var clientesMap = Map[Int, ActorRef]()

  private var sistema: Option[ActorSystem] = None
  private var onEstadoCambiadoHost: Option[Juego => Unit] = None

  def registrarUIHost(cb: Juego => Unit): Unit =
    onEstadoCambiadoHost = Some(cb)

  def ipLocal: String =
    Try(InetAddress.getLocalHost.getHostAddress).getOrElse("127.0.0.1")

  def clientesConectados: Int = clientesMap.size
  def estadoActualSnapshot: Juego = estadoActual

  def iniciar(onClienteConectado: Int => Unit): Unit =
    val config = ConfigFactory.parseString(s"""
      pekko {
        actor {
          provider = remote
          allow-java-serialization = on
        }
        remote.artery {
          transport = tcp
          canonical.hostname = "$ipLocal"
          canonical.port = $puerto
        }
      }
    """)
    val sys = ActorSystem("SistemaSaboteurHost", config)
    this.sistema = Some(sys)

    class ActorServidorInternal extends Actor:
      def receive: Receive =
        case "SolicitudUnirse" =>
          val clienteRef = sender()
          val idAsignado = juegoInicial.listaJugadores
            .map(_.id)
            .filterNot(_ == hostId)
            .find(id => !clientesMap.contains(id))
            .getOrElse(-1)

          if idAsignado != -1 then
            clientesMap = clientesMap + (idAsignado -> clienteRef)
            clienteRef ! BienvenidaMsg(idAsignado)
            clienteRef ! EstadoJuegoMsg(estadoActual)
            Platform.runLater { onClienteConectado(idAsignado) }

        case msg: MensajeRed =>
          clientesMap.find(_._2 == sender()).map(_._1).foreach { id =>
            Platform.runLater {
              procesarAccionHost(msg, id, notificarHostUI = true) match
                case ResultadoAccion.Error(razon) => 
                  clientesMap.get(id).foreach(_ ! ErrorMsg(razon))
                case _ => ()
            }
          }

    sys.actorOf(Props(new ActorServidorInternal), "servidor")
    println(s"[Pekko Host] Servidor activo en $ipLocal:$puerto")

  def procesarAccionHost(msg: MensajeRed, jugadorId: Int, notificarHostUI: Boolean = false): ResultadoAccion =
    synchronized {
      val resultado = aplicarAccionAJuego(estadoActual, msg, jugadorId)
      resultado match
        case ResultadoAccion.Exito(nuevoJuego, _) =>
          estadoActual = nuevoJuego
          // El mensajePrivado (ej: resultado de la Lupa) solo va al jugador que actuó.
          // A los demás clientes se les manda el juego con ese campo limpio.
          val juegoSinPrivado = nuevoJuego.copy(mensajePrivado = "")
          clientesMap.foreach { case (clienteId, ref) =>
            if clienteId == jugadorId then ref ! EstadoJuegoMsg(nuevoJuego)
            else                            ref ! EstadoJuegoMsg(juegoSinPrivado)
          }
          // El host recibe el estado completo SOLO si fue el host quien actuó.
          // Si actuó un cliente, el host es "otro jugador" y debe ver la versión limpia.
          if notificarHostUI then
            val juegoParaHost = if jugadorId == hostId then nuevoJuego else juegoSinPrivado
            onEstadoCambiadoHost.foreach(_(juegoParaHost))
        case _ => ()
      resultado
    }

// ─────────────────────────────────────────────
//  CLIENTE — MODIFICADO PARA LAN
// ─────────────────────────────────────────────
class ClienteJuego(host: String, puerto: Int):

  private var sistema: Option[ActorSystem] = None
  private var actorCliente: Option[ActorRef] = None

  @volatile private var manejadorMensaje: MensajeRed => Unit = _ => ()
  @volatile private var manejadorError:   String => Unit     = _ => ()

  // Ahora el cliente también detecta su propia IP de Wi-Fi para que el Host sepa cómo responderle
  def ipLocal: String =
    Try(InetAddress.getLocalHost.getHostAddress).getOrElse("127.0.0.1")

  def conectar(onMensaje: MensajeRed => Unit, onError: String => Unit): Unit =
    this.manejadorMensaje = onMensaje
    this.manejadorError = onError

    // Corregido: canonical.hostname ahora usa la IP real del cliente en el Wi-Fi
    val config = ConfigFactory.parseString(s"""
      pekko {
        actor {
          provider = remote
          allow-java-serialization = on
        }
        remote.artery {
          transport = tcp
          canonical.hostname = "$ipLocal"
          canonical.port = 0
        }
      }
    """)
    val sys = ActorSystem("SistemaSaboteurCliente", config)
    this.sistema = Some(sys)

    class ActorClienteInternal extends Actor:
      private val servidorSelection = context.actorSelection(s"pekko://SistemaSaboteurHost@$host:$puerto/user/servidor")

      override def preStart(): Unit =
        servidorSelection ! "SolicitudUnirse"

      def receive: Receive =
        case msg: EstadoJuegoMsg => Platform.runLater { manejadorMensaje(msg) }
        case msg: ErrorMsg       => Platform.runLater { manejadorMensaje(msg) }
        case msg: BienvenidaMsg  => Platform.runLater { manejadorMensaje(msg) }
        case msg: MensajeRed     => servidorSelection ! msg
    
    this.actorCliente = Some(sys.actorOf(Props(new ActorClienteInternal), "cliente"))

  def alRecibir(nuevoManejador: MensajeRed => Unit): Unit =
    this.manejadorMensaje = nuevoManejador

  def alPerderConexion(nuevoManejador: String => Unit): Unit =
    this.manejadorError = nuevoManejador

  def enviarAccion(accion: MensajeRed): Unit =
    actorCliente.foreach(_ ! accion)