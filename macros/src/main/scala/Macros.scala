import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object HelloMacro {
  def impl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    annottees match {
      case (classDecl: ClassDef) :: Nil =>
        val q"class $name extends ..$bases { ..$body }" = classDecl

        q"""
        class $name extends ..$bases {
          ..$body
          def hello = "Hello"
        }
        """

      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}

class hello extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro HelloMacro.impl
}