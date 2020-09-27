functor NaiveBayes (
  structure Seq: SEQUENCE
) :> NB where 'a seq = 'a Seq.t =
struct
  
  type 'a seq = 'a Seq.t
  datatype feature = Disc of int | Cont
  datatype prob = DiscProb of real seq | ContProb of real * real
  type nb = prob seq seq * real seq
  
  exception Range
  exception Unimplemented

  fun seq2 s = Seq.map (fn x => x ** 2) s
  fun seqminus (x,y) = Seq.zipWith (op-) (x,y)

  fun fit 
    (ft : feature seq, nl : int)
    (ds : int seq seq)
    (ls : int seq) : nb =
    let
      val numft = Seq.length ft     (* number of features *)
      val numcases = Seq.length ds  (* number of cases*)
      
      (* ds by label *)
      val dslabel = 
        let
          fun isLabel c (i, x) = (Seq.nth ls i) = c
          fun filterByLabel c = Seq.filterIdx (isLabel c) ds
        in
          Seq.tabulate filterByLabel nl
        end

      val dslabelt = 
        let
          fun getIthFeature i = Seq.map (fn l => Seq.nth l i) ds
          fun transpose s = Seq.tabulate getIthFeature numft
        in
          Seq.map transpose dslabel
        end
      
      fun getIthParam (c : int) (i : int) : prob = 
        let
          val seqci = Seq.nth (Seq.nth dslabelt c) i
          val len = Real.fromInt (Seq.length seqci)
          fun equalJ j x = x = j
          fun getValueNumJ j = 
            Real.fromInt (Seq.length (Seq.filter (equalJ j) seqci))
          fun getProbJ j = (getValueNumJ j) / len
          fun getMean s = Real.fromInt (Seq.reduce (op+) 0 s)
          fun getVar m s = seq2 (seqminus (s, m))
        in
          case Seq.nth i ft of
               Disc n => DiscProb (Seq.tabulate getProbJ seqci)
             | Cont => let m = getMean ith in ContProb (m, getVar m ith) end
        end

      val params : prob seq seq = 
        Seq.tabulate (fn c => Seq.tabulate (getIthParam c) numft) nl
      val priors : real seq =
        let
          fun equalC c x = x = c
          val lengthC = Seq.length (Seq.filter (equalC c) ls)
          fun getProbC c = (Real.fromInt lengthC) / (Real.fromInt numcases)
        in
          Seq.tabulate getProbC nl
        end
    in
      (params, priors)
    end

  fun getPrior m = let val (_, p) = m in p end
  fun getParam m = 
    let
      val (p, _) = m
    in
      raise Unimplemented
    end

end
