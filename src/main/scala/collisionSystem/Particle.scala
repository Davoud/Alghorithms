package collisionSystem

import edu.princeton.cs.algs4.{StdDraw, StdRandom}
import java.awt.Color

class Particle(private var rx: Double, private var ry: Double,
               private var vx: Double, private var vy: Double,
               private val radius: Double, private val mass: Double,
               private val color: Color) {
	
	def this() {
		this(
			rx = StdRandom.uniform(0.0, 1.0),
			ry = StdRandom.uniform(0.0, 1.0),
			vx = StdRandom.uniform(-0.005, 0.005),
			vy = StdRandom.uniform(-0.005, 0.005),
			radius = 0.02,
			mass = 0.5,
			color = Color.BLACK
		)
	}
	
	val INFINITY = Double.PositiveInfinity
	private var _count: Int = 0
	
	def count = _count
	
	def move(dt: Double): Unit = {
		rx = rx + vx * dt
		ry = ry + vy * dt
	}
	
	def draw(): Unit = {
		StdDraw.setPenColor(color)
		StdDraw.filledCircle(rx, ry, radius)
	}
	
	
	def timeToHit(that: Particle): Double = {
		if (this eq that) return INFINITY
		val dx = that.rx - this.rx
		val dy = that.ry - this.ry
		val dvx = that.vx - this.vx
		val dvy = that.vy - this.vy
		val dvdr = dx * dvx + dy * dvy
		if (dvdr > 0) return INFINITY
		val dvdv = dvx * dvx + dvy * dvy
		val drdr = dx * dx + dy * dy
		val sigma = this.radius + that.radius
		val d = (dvdr * dvdr) - dvdv * (drdr - sigma * sigma)
		if (d < 0) return INFINITY
		-(dvdr + Math.sqrt(d)) / dvdv
	}
	
	
	def timeToHitVerticalWall: Double =
		if (vx > 0) (1.0 - rx - radius) / vx
		else if (vx < 0) (radius - rx) / vx
		else INFINITY
	
	def timeToHitHorizontalWall: Double =
		if (vy > 0) (1.0 - ry - radius) / vy
		else if (vy < 0) (radius - ry) / vy
		else INFINITY
	
	def bounceOff(that: Particle): Unit = {
		val dx = that.rx - this.rx
		val dy = that.ry - this.ry
		val dvx = that.vx - this.vx
		val dvy = that.vy - this.vy
		val dvdr = dx * dvx + dy * dvy // dv dot dr
		val dist = this.radius + that.radius // distance between particle centers at collison
		
		// magnitude of normal force
		val magnitude = 2 * this.mass * that.mass * dvdr / ((this.mass + that.mass) * dist)
		
		// normal force, and in x and y directions
		val fx = magnitude * dx / dist
		val fy = magnitude * dy / dist
		
		// update velocities according to normal force
		this.vx += fx / this.mass
		this.vy += fy / this.mass
		that.vx -= fx / that.mass
		that.vy -= fy / that.mass
		
		// update collision counts
		this._count += 1
		that._count += 1
	}
	
	def bounceOffVerticalWall(): Unit = {
		vx = -vx
		_count += 1
	}
	
	def bounceOffHorizontalWall(): Unit = {
		vy = -vy
		_count += 1
	}
	
	def kineticEnergy: Double = 0.5 * mass * (vx * vx + vy * vy)
	
	
}
