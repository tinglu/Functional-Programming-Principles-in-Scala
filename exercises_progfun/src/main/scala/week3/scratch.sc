/*
*
* Class organisation
* */

import week3.Rational // named imports

new Rational(1, 2)



/*
Traits actually much more powerful than interfaces in Java
because traits can contain fields and concrete methods
whereas interfaces can contain only abstract methods.
Traits never have parameters; only classes can
*/

def error(msg: String) = throw new Error(msg)

//error("test")


val x = null
val y: String = x
//val z: Int = x // type mismatch