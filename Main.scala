import scala.util.Random

enum Rol:
  case BUSCADOR, SABOTEADOR

enum Herramienta:
  case PICO, CARRETILLA, FAROL

enum TipoAccion:
  case SABOTAJE(herramienta: Herramienta)
  case REPARACION(herramientas: List[Herramienta])
  case MAPA
  case DERRUMBE

sealed trait Carta {
  def id: Int
  def nombre: String
}

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

case class Jugador(
    id: Int,
    nombre: String,
    rol: Rol,
    mano: List[Carta],
    herramientasRotas: List[Herramienta] = Nil
) {
  def estaBloqueado(): Boolean = herramientasRotas.nonEmpty
  
  def eliminarCartaDeMano(cartaId: Int): Jugador = {
    this.copy(mano = this.mano.filterNot(_.id == cartaId))
  }
}

case class Posicion(x: Int, y: Int) {
  def arriba: Posicion    = Posicion(x, y - 1)
  def abajo: Posicion     = Posicion(x, y + 1)
  def izquierda: Posicion = Posicion(x - 1, y)
  def derecha: Posicion   = Posicion(x + 1, y)
}

case class Tablero(
    cuadricula: Map[Posicion, CartaTunel],
    posicionInicio: Posicion,
    posicionesMeta: List[Posicion]
) {
  def validarColocacion(pos: Posicion, nuevaCarta: CartaTunel): Boolean = {
    
    if (cuadricula.contains(pos)) return false

    val vecinoArriba = cuadricula.get(pos.arriba)
    val vecinoAbajo  = cuadricula.get(pos.abajo)
    val vecinoIzq    = cuadricula.get(pos.izquierda)
    val vecinoDer    = cuadricula.get(pos.derecha)

    val tieneVecino = vecinoArriba.isDefined || vecinoAbajo.isDefined || vecinoIzq.isDefined || vecinoDer.isDefined
    if (!tieneVecino) return false

    val coincideArriba = vecinoArriba.forall(v => v.estaOculta || v.abajo == nuevaCarta.arriba)
    val coincideAbajo  = vecinoAbajo.forall(v => v.estaOculta || v.arriba == nuevaCarta.abajo)
    val coincideIzq    = vecinoIzq.forall(v => v.estaOculta || v.derecha == nuevaCarta.izquierda)
    val coincideDer    = vecinoDer.forall(v => v.estaOculta || v.izquierda == nuevaCarta.derecha)

    coincideArriba || coincideAbajo || coincideIzq || coincideDer
  }
}

case class Mazo(cartasRobo: List[Carta], cartasDescarte: List[Carta])

case class Juego(
    listaJugadores: List[Jugador],
    tablero: Tablero,
    mazo: Mazo,
    turnoActual: Int,
    mensajeAlerta: String = ""
) {
  def inicializarPartida(): Juego = {
    var mazoActual = mazo.cartasRobo
    val jugadoresConCartas = listaJugadores.map { jugador =>
      val (mano, restoMazo) = mazoActual.splitAt(6)
      mazoActual = restoMazo
      jugador.copy(mano = mano)
    }
    this.copy(listaJugadores = jugadoresConCartas, mazo = Mazo(mazoActual, Nil))
  }

  def avanzarTurnoYRobar(jugadorId: Int, nuevoTablero: Tablero, cartaIdUsada: Int, alerta: String = ""): Juego = {
    val jugadorIndex = listaJugadores.indexWhere(_.id == jugadorId)
    if (jugadorIndex == -1) return this

    val jugadorActual = listaJugadores(jugadorIndex)
    val jugadorSinCarta = jugadorActual.eliminarCartaDeMano(cartaIdUsada)
    
    val (nuevaMano, nuevoMazoRobo) = mazo.cartasRobo match {
      case siguienteCarta :: restoDelMazo => (jugadorSinCarta.mano :+ siguienteCarta, restoDelMazo)
      case Nil => (jugadorSinCarta.mano, Nil)
    }

    val jugadorActualizado = jugadorActual.copy(mano = nuevaMano)
    val nuevaListaJugadores = listaJugadores.updated(jugadorIndex, jugadorActualizado)

    this.copy(
      tablero = nuevoTablero,
      listaJugadores = nuevaListaJugadores,
      mazo = Mazo(nuevoMazoRobo, mazo.cartasDescarte),
      turnoActual = this.turnoActual + 1,
      mensajeAlerta = alerta
    )
  }
}

object GeneradorMazo {
  def crearMazoMezclado(): Mazo = {
    var idSecuencia = 1
    
    val tuneles = (1 to 41).map { i =>
      val id = idSecuencia; idSecuencia += 1
      if (i <= 15) CartaTunel(id, "Túnel Cruz", true, true, true, true, false)
      else if (i <= 25) CartaTunel(id, "Túnel Recto H", false, false, true, true, false)
      else if (i <= 35) CartaTunel(id, "Túnel Recto V", true, true, false, false, false)
      else CartaTunel(id, "Callejón Cruz X", true, true, true, true, true)
    }.toList

    val sabotajes = Herramienta.values.flatMap { h =>
      (1 to 3).map { _ =>
        val id = idSecuencia; idSecuencia += 1
        CartaAccion(id, s"Sabotaje ($h)", TipoAccion.SABOTAJE(h))
      }
    }.toList

    val reparacionesIndividuales = Herramienta.values.flatMap { h =>
      (1 to 2).map { _ =>
        val id = idSecuencia; idSecuencia += 1
        CartaAccion(id, s"Reparar ($h)", TipoAccion.REPARACION(List(h)))
      }
    }.toList

    val reparacionesHibridas = List(
      CartaAccion(idSecuencia, "Reparar Pico/Carretilla", TipoAccion.REPARACION(List(Herramienta.PICO, Herramienta.CARRETILLA))),
      CartaAccion(idSecuencia + 1, "Reparar Pico/Farol", TipoAccion.REPARACION(List(Herramienta.PICO, Herramienta.FAROL))),
      CartaAccion(idSecuencia + 2, "Reparar Carretilla/Farol", TipoAccion.REPARACION(List(Herramienta.CARRETILLA, Herramienta.FAROL)))
    )
    idSecuencia += 3

    val mapas = (1 to 6).map { _ =>
      val id = idSecuencia; idSecuencia += 1
      CartaAccion(id, "Lupa (Mapa)", TipoAccion.MAPA)
    }.toList

    val derrumbes = (1 to 3).map { _ =>
      val id = idSecuencia; idSecuencia += 1
      CartaAccion(id, "Derrumbamiento", TipoAccion.DERRUMBE)
    }.toList

    Mazo(Random.shuffle(tuneles ++ sabotajes ++ reparacionesIndividuales ++ reparacionesHibridas ++ mapas ++ derrumbes), Nil)
  }
}