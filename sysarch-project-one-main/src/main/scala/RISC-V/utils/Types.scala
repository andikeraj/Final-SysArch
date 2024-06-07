package RISCV.utils

object Types {
    // Scala supports type-level 'and' by using 'with'.
    // But we can get 'or' by using de Morgan's law.
    type NOT[A] = A => Nothing
    type OR[T, U] = NOT[NOT[T] with NOT[U]]
    type NOTNOT[A] = NOT[NOT[A]]
    type ORTYPE[T, U] = { type check[X] = NOTNOT[X] <:< (T OR U) } 
}