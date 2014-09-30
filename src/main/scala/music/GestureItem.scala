package music

import music.Common._

//lin, sin, exp, wel, sqr, cub

// lin, lin line
// sin, sin halfsoft
// exp, exp sharpest
// wel, wel soft
// sqr, sqr halfsharp
// cub, cub sharp



sealed case class AttackTime(value: Float)
object SHARP_INVPHI_TIME extends AttackTime(1 - invPhi)
object HALF_TIME extends AttackTime(0.5f)
object SOFT_INVPHI_TIME extends AttackTime(invPhi)

sealed case class EnvCurve(name: Symbol)
object LINEAR extends EnvCurve('line)
object SINE extends EnvCurve('sin)
object EXPONENTIAL extends EnvCurve('exp)
object WELCH extends EnvCurve('wel)
object SQUARED extends EnvCurve('sqr)
object CUBED extends EnvCurve('cub)

case class GestureItem(amplitude: Float, attackTime: Float, attackCurve: EnvCurve, decayCurve: EnvCurve)
