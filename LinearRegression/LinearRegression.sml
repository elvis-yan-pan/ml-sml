functor LinearRegression (
  structure Sequence : SEQUENCE
) :> LinearRegression where Seq = Sequence =
struct
  structure Seq = Sequence

  type 'a seq = 'a Seq.t
  type lr = real seq

  exception Range
  exception Unimplemented

  fun fit x y =
    let
      val n = Seq.length (Seq.nth x 0)
      val w = Seq.tabulate (fn _ => 0.0) (n+1)
      fun gradientDescent w = raise Unimplemented
    in

    end

end
