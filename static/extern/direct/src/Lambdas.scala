package LOWERCASE_DSL_NAME.direct

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

/**
 * LambdaOps allow reifying anonymous functions so that they can be used in generated code.
 *
 * This functionality should be used with care, since higher-order functions add overhead
 * and are not supported on all programming models or hardware targets.
 */
trait LambdaOps //extends Functions 
trait LambdaCompilerOps //extends LambdaOps
