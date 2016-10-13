Examples from other talks:
* Coastal management (too boring?)
* FLABLoM Matrix type (too hard to explain?)
* DSLM: some maths example(?)
* DSLM: rekryteringspresentation (på svenska) ghttp:/github.com/DSLM/DSLsofMath/presentation/
* (Grundläggande datateknik: IT läser den i LP1)
* (Java: IT läser det i LP1)
* ValiantAgda: Too complex: http://www.cse.chalmers.se/~patrikj/talks/IFIP2.1ZeegseJansson_ParParseAlgebra.org
* John's invited talk in the FP course: http://www.cse.chalmers.se/edu/course/TDA555/Material/Guest_John/John2012.pdf

Google for Hackage + Chalmers
* Lava:                https://hackage.haskell.org/package/chalmers-lava2000
    *                  https://mail.haskell.org/pipermail/haskell/2008-August/020581.html
* QuickCheck:          https://hackage.haskell.org/package/QuickCheck
* syntactic:           https://hackage.haskell.org/package/syntactic
* ChasingBottoms:      https://hackage.haskell.org/package/ChasingBottoms
* tf-random:           https://hackage.haskell.org/package/tf-random
* language-vhdl:       https://hackage.haskell.org/package/language-vhdl
* cereal:              https://hackage.haskell.org/package/cereal
* Wired                https://hackage.haskell.org/package/Wired
* haskell-src-exts     https://hackage.haskell.org/package/haskell-src-exts
* pesca                https://hackage.haskell.org/package/pesca
* jukebox              https://hackage.haskell.org/package/jukebox
* structural-induction https://hackage.haskell.org/package/structural-induction
* mac                  https://hackage.haskell.org/package/mac
* ho-rewriting         https://hackage.haskell.org/package/ho-rewriting
* feldspar-language    https://hackage.haskell.org/package/feldspar-language
* quickcheck-script    https://hackage.haskell.org/package/quickcheck-script
* Bookshelf            https://hackage.haskell.org/package/Bookshelf
* BNFC-meta            https://hackage.haskell.org/package/BNFC-meta
* Agda                 https://hackage.haskell.org/package/Agda
* http://bnfc.digitalgrammars.com/
* PFP course
* Haste: http://haste-lang.org/blog/
* Testing Type Class Laws



----------------------------------------------------------------

* Coastal management http://www.cse.chalmers.se/~patrikj/talks/WP4_DSL_Y1.5.pdf

```Haskell
coastalManagement = do
  -- nodes
  seaLevelRise    <- mkNode "sea level rise"
  riskOfFlooding  <- mkNode "risk of flooding"
  flooding        <- mkNode "flooding"
  investments     <- mkNode "investments"
  measuresToPreventFlooding <- mkNode "measures to prevent flooding"
  ecologyInTheCoastalZone   <- mkNode "ecology in the coastal zone"
  -- edges
  link seaLevelRise >+> riskOfFlooding >+> flooding
                    >+> measuresToPreventFlooding
  link measuresToPreventFlooding >-> ecologyInTheCoastalZone
  link measuresToPreventFlooding >-> riskOfFlooding
  link investments >+> measuresToPreventFlooding
```

* FLABLoM Matrix Type

\begin{code}
data M (a : Set) : (rows cols : Shape) → Set where
  One :  a → M a L L

  Col :  {r₁ r₂ : Shape} →
         M a r₁ L → M a r₂ L → M a (B r₁ r₂) L

  Row :  {c₁ c₂ : Shape} →
         M a L c₁ → M a L c₂ →  M a L (B c₁ c₂)

  Q   :  {r₁ r₂ c₁ c₂ : Shape} →
         M a r₁ c₁ → M a r₁ c₂ →
         M a r₂ c₁ → M a r₂ c₂ →
         M a (B r₁ r₂) (B c₁ c₂)
\end{code}
\end{frame}
