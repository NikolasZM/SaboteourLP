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
    tieneOro: Boolean = false
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
  def arriba: Posicion    = Posicion(x, y - 1) // En gráficos FX, restar en Y sube
  def abajo: Posicion     = Posicion(x, y + 1) // Sumar en Y baja
  def izquierda: Posicion = Posicion(x - 1, y)
  def derecha: Posicion   = Posicion(x + 1, y)
}

case class Tablero(
    cuadricula: Map[Posicion, CartaTunel],
    posicionInicio: Posicion,
    metas: List[Posicion]
) {
  def validarColocacion(pos: Posicion, nuevaCarta: CartaTunel): Boolean = {
    if (cuadricula.contains(pos)) return false

    val vecinos = List(
      (pos.arriba,    (c: CartaTunel) => c.abajo,     nuevaCarta.arriba),
      (pos.abajo,     (c: CartaTunel) => c.arriba,    nuevaCarta.abajo),
      (pos.izquierda, (c: CartaTunel) => c.derecha,   nuevaCarta.izquierda),
      (pos.derecha,   (c: CartaTunel) => c.izquierda, nuevaCarta.derecha)
    )

    val tieneVecino = vecinos.exists { case (vecinoPos, _, _) => cuadricula.contains(vecinoPos) }
    if (!tieneVecino) return false

    vecinos.forall { case (vecinoPos, obtenerBordeVecino, bordeNuevaCarta) =>
      cuadricula.get(vecinoPos) match {
        case Some(cartaVecina) => obtenerBordeVecino(cartaVecina) == bordeNuevaCarta
        case None => true
      }
    }
  }
}

case class Mazo(cartasRobo: List[Carta], cartasDescarte: List[Carta])

case class Juego(
    listaJugadores: List[Jugador],
    tablero: Tablero,
    mazo: Mazo,
    turnoActual: Int
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

  // --- NUEVA FUNCIÓN PURA PARA AVANZAR EL TURNO Y ROBAR ---
  def avanzarTurnoYRobar(jugadorId: Int): Juego = {
    val jugadorIndex = listaJugadores.indexWhere(_.id == jugadorId)
    if (jugadorIndex == -1) return this

    val jugadorActual = listaJugadores(jugadorIndex)
    
    // Verificamos si quedan cartas en el mazo para robar
    val (nuevaMano, nuevoMazoRobo) = mazo.cartasRobo match {
      case siguienteCarta :: restoDelMazo => 
        // Si hay cartas, se le agrega a la mano actual
        (jugadorActual.mano :+ siguienteCarta, restoDelMazo)
      case Nil => 
        // Si el mazo está vacío, la mano se queda como está (irá disminuyendo)
        (jugadorActual.mano, Nil)
    }

    val jugadorActualizado = jugadorActual.copy(mano = nuevaMano)
    val nuevaListaJugadores = listaJugadores.updated(jugadorIndex, jugadorActualizado)

    // Devolvemos el nuevo estado del juego incrementando el contador de turnos
    this.copy(
      listaJugadores = nuevaListaJugadores,
      mazo = Mazo(nuevoMazoRobo, mazo.cartasDescarte),
      turnoActual = this.turnoActual + 1
    )
  }
}

object GeneradorMazo {
  def crearMazoMezclado(): Mazo = {
    var idSecuencia = 1
    
    val tuneles = (1 to 41).map { i =>
      val id = idSecuencia; idSecuencia += 1
      CartaTunel(id, s"Túnel $i", arriba = true, abajo = true, izquierda = true, derecha = true, esCallejonSinSalida = false)
    }.toList

    val sabotajes = Herramienta.values.flatMap { h =>
      (1 to 3).map { _ =>
        val id = idSecuencia; idSecuencia += 1
        CartaAccion(id, s"Sabotaje $h", TipoAccion.SABOTAJE(h))
      }
    }.toList

    val reparacionesIndividuales = Herramienta.values.flatMap { h =>
      (1 to 2).map { _ =>
        val id = idSecuencia; idSecuencia += 1
        CartaAccion(id, s"Reparar $h", TipoAccion.REPARACION(List(h)))
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

@main def probarLogicaJuego(): Unit = {
  println("Lógica lista para usarse desde la Interfaz.")
}