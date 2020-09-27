functor NaiveBayes (
  structure Seq: SEQUENCE
) :> NB where 'a seq = 'a Seq.t =
struct
  
  type 'a seq = 'a Seq.t
  datatype feature = Disc of int | Cont
  datatype prob = DiscProb of real seq | ContProb of real * real
  type nb = prob seq * real seq
  
  exception Range
  exception Unimplemented

  fun seq2 s = Seq.map (fn x => x ** 2) s
  fun seqminus (x,y) = Seq.zipWith (op-) (x,y)

  fun fit (ft, nl) ds ls =
    let
      val numft = Seq.length ft
      val numcases = Seq.length ds
      val dst = Seq.tabulate (fn i => Seq.map (fn l => Seq.nth l i) ds) numft
      fun fiter n (s, c) = Seq.nth n s
      
      fun getIthParam i = 
        let
          val ith = Seq.nth dst i
          fun ftab j = Real.fromInt (Seq.length (Seq.filter (fn x => x = j) s))
          fun getMean s = Real.fromInt (Seq.reduce (op+) 0 s)
          fun getVar m s = seq2 (seqminus (s, m))
        in
          case Seq.nth i ft of
               Disc n => Seq.tabulate (fn j => ftab / (Real.fromInt ds)) ith
             | Cont => let m = getMean ith in (m, getVar m ith)
        end

      val params = raise Unimplemented
    in
      raise Unimplemented
    end

end
