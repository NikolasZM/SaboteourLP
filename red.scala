//> using dep "org.apache.pekko::pekko-actor:1.0.2"
//> using dep "org.apache.pekko::pekko-remote:1.0.2"
//> using dep "com.typesafe:config:1.4.3"


import java.net.InetAddress
import scala.util.Try
import scalafx.application.Platform
import org.apache.pekko.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

// ─────────────────────────────────────────────────────────────────────────────
// 1. PROTOCOLO DE MENSAJES (ADT Inmutable y Serializable)
// ─────────────────────────────────────────────────────────────────────────────
// Agregamos 'java.io.Serializable' para que Pekko pueda enviar los objetos por la red LAN
sealed trait MensajeRed extends java.io.Serializable

// Cliente → Host
case class AccionColocarTunel(cartaId: Int, posX: Int, posY: Int, voltear: Boolean) extends MensajeRed
case class AccionSabotaje(cartaId: Int, objetivoId: Int) extends MensajeRed
case class AccionReparacion(cartaId: Int, objetivoId: Int) extends MensajeRed
case class AccionMapa(cartaId: Int, posX: Int, posY: Int) extends MensajeRed
case class AccionDerrumbe(cartaId: Int, posX: Int, posY: Int) extends MensajeRed
case class AccionDescartar(cartaId: Int) extends MensajeRed
case class UnirsePartida(nombre: String) extends MensajeRed

// Host → Cliente
case class EstadoJuegoMsg(juego: Juego) extends MensajeRed
case class ErrorMsg(razon: String) extends MensajeRed
case class AsignarIdMsg(id: Int) extends MensajeRed

// Mensajes internos de control para el Servidor
private case class Broadcast(msg: MensajeRed)
private case class EnviarA(clienteId: Int, msg: MensajeRed)
private case class CambiarManejador(nuevo: MensajeRed => Unit)

// ─────────────────────────────────────────────────────────────────────────────
// 2. COMPONENTE HOST (SERVIDOR CENTRAL)
// ─────────────────────────────────────────────────────────────────────────────
object Host:
  var puerto: Int = 25565
  var ip: String = "127.0.0.1"

  private var sistema: Option[ActorSystem] = None
  private var actorHost: Option[ActorRef] = None

  def arrancar(puertoDeInterfaz: Int, juegoInicial: Juego, onMensaje: (Int, MensajeRed) => Unit): Unit =
    this.puerto = puertoDeInterfaz
    // Detectamos la IP real de tu máquina en la red local (WiFi/Ethernet)
    this.ip = Try(InetAddress.getLocalHost.getHostAddress).getOrElse("127.0.0.1")

    // Configuración integrada de Pekko de forma simple (sin archivos externos)
    val config = ConfigFactory.parseString(s"""
      pekko {
        actor { provider = remote; allow-java-serialization = on }
        remote.artery { transport = tcp; canonical.hostname = "${this.ip}"; canonical.port = $puerto }
      }
    """)

    val sys = ActorSystem("SistemaSaboteurHost", config)
    this.sistema = Some(sys)
    // Instanciamos el Actor del Servidor encargado de la concurrencia
    this.actorHost = Some(sys.actorOf(Props(new ActorServidor(onMensaje)), "servidor"))
    println(s"[Pekko Host] Corriendo de forma asíncrona en ${this.ip}:$puerto")

  def broadcast(msg: MensajeRed): Unit = 
    actorHost.foreach(_ ! Broadcast(msg))

  def enviarA(clienteId: Int, msg: MensajeRed): Unit = 
    actorHost.foreach(_ ! EnviarA(clienteId, msg))

// El Actor Servidor que procesa los mensajes concurrentes
class ActorServidor(onMensaje: (Int, MensajeRed) => Unit) extends Actor:
  private var clientes = Map[Int, ActorRef]()
  private var proxId = 1

  def receive: Receive =
    case UnirsePartida(nombre) =>
      val idAsignado = proxId
      proxId += 1
      // El método 'sender()' nos da la dirección de red del cliente automáticamente
      clientes = clientes + (idAsignado -> sender())
      onMensaje(idAsignado, UnirsePartida(nombre))

    case Broadcast(msg) =>
      clientes.values.foreach(_ ! msg)

    case EnviarA(id, msg) =>
      clientes.get(id).foreach(_ ! msg)

    case msg: MensajeRed =>
      // Buscamos qué ID numérico tiene asignado el actor que nos mandó la jugada
      clientes.find(_._2 == sender()).map(_._1).foreach { id =>
        onMensaje(id, msg)
      }

// ─────────────────────────────────────────────────────────────────────────────
// 3. COMPONENTE CLIENTE (JUGADORES)
// ─────────────────────────────────────────────────────────────────────────────
object Cliente:
  var host: String = "127.0.0.1"
  var puerto: Int = 25565

  private var sistema: Option[ActorSystem] = None
  private var actorCliente: Option[ActorRef] = None
  private var manejadorMensaje: MensajeRed => Unit = _ => ()

  def configurar(h: String, p: Int): Unit =
    this.host = h
    this.puerto = p

  def conectar(onMensaje: MensajeRed => Unit, onError: String => Unit): Unit =
    this.manejadorMensaje = onMensaje

    val config = ConfigFactory.parseString("""
      pekko {
        actor { provider = remote; allow-java-serialization = on }
        remote.artery { transport = tcp; canonical.hostname = "127.0.0.1"; canonical.port = 0 }
      }
    """)

    val sys = ActorSystem("SistemaSaboteurCliente", config)
    this.sistema = Some(sys)
    this.actorCliente = Some(sys.actorOf(Props(new ActorCliente(host, puerto, manejadorMensaje, onError)), "cliente"))

  def enviar(msg: MensajeRed): Unit = 
    actorCliente.foreach(_ ! msg)

  def alRecibir(nuevoManejador: MensajeRed => Unit): Unit =
    this.manejadorMensaje = nuevoManejador
    actorCliente.foreach(_ ! CambiarManejador(nuevoManejador))

  def alPerderConexion(nuevoManejador: String => Unit): Unit = ()

// El Actor Cliente encargado de reaccionar a la red
class ActorCliente(hostServer: String, puertoServer: Int, var onMensaje: MensajeRed => Unit, onError: String => Unit) extends Actor:
  // Conectamos remotamente al buzón del servidor usando su ruta única de red
  private val servidorSelection = context.actorSelection(s"pekko://SistemaSaboteurHost@$hostServer:$puertoServer/user/servidor")

  def receive: Receive =
    case CambiarManejador(nuevo) =>
      this.onMensaje = nuevo

    // Si viene del Servidor, actualizamos la interfaz gráfica de forma reactiva en el hilo de la UI
    case msg: EstadoJuegoMsg => Platform.runLater { onMensaje(msg) }
    case msg: ErrorMsg       => Platform.runLater { onMensaje(msg) }
    case msg: AsignarIdMsg   => Platform.runLater { onMensaje(msg) }
    
    // Si la interfaz local nos da una acción de juego, la enviamos por red al Servidor
    case msg: MensajeRed     => servidorSelection ! msg
