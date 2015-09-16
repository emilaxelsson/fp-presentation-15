% Functional Programming @ D&IT



Test slide 2
====================================================================================================

Incremental list:

  <div class="incremental">

* First item

  </div>
  <div class="incremental">

* Second item
    * Sub-item

  </div>



Test slide 2
================================================================================

Haskell code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
ones = 1:ones
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Funktionell Programmering på D&IT
================

* Personer
* Kurser
* Forskning
* Företag
* Historia


**Pre-writing:**
================

Personer
================

* John Hughes: [on wikipedia](https://en.wikipedia.org/wiki/John_Hughes_(computer_scientist))
* Mary Sheeran: [ICFP keynote 2015](https://www.youtube.com/watch?v=gz8JpdAwtuo&index=15&list=PLnqUlCo055hWNtUo1Haoq347VhCqIjs7u) [slides](http://icfpconference.org/icfp2015/sheeran-keynote.pdf)
* Koen Claessen: [QuickCheck](https://en.wikipedia.org/wiki/QuickCheck), Aktuellt: kursen [Diskret matematik för datavetare](http://www.cse.chalmers.se/edu/course/DIT980/)
* Patrik Jansson: Står här! [Github](https://github.com/patrikja) Aktuellt: [ProfLect](https://github.com/patrikja/ProfLect)
* Emil Axelsson:
* Niklas Broberg:

TODO: fyll på med fler ...
TODO: fyll på med mer information


Kurser som innehåller FP
================

* Kandidatnivån (första tre åren):
    * LP1: IFP = [Introduktion till Funktionell Programmering](http://www.cse.chalmers.se/edu/course/TDA555/), Emil Axelsson
    * LP2: FP = [Functional Programming](www.cse.chalmers.se/edu/course/TDA452/), Dave Sands
    * LP3: DSLM = [Domain Specific Languages of Mathematics](https://github.com/DSLsofMath), Cezar Ionescu, Patrik Jansson
    * (LP3: ProgPara = [Programming Paradigms](http://www.cse.chalmers.se/~bernardy/pp/), Jean-Philippe Bernardy, inställd våren 2015)
    * LP3-4: Kandidatarbete
    * ...
* Masternivån (sista två åren):
    * LP1: Types = [http://www.cse.chalmers.se/edu/year/2013/course/DAT140_Types/](http://www.cse.chalmers.se/edu/course/DAT140/), Thierry Coquand, Peter Dybjer, Ulf Norell
    * LP2: PLT = [Andreas Abel](http://www.cse.chalmers.se/edu/course/DAT151_Programming_Language_Technology/)
    * LP2: Models = [Models of Computation](https://sites.google.com/site/modelsofcomputation/)
    * LP3: AFP = [Advanced Functional Programming](http://www.cse.chalmers.se/edu/course/afp/), Patrik Jansson
    * LP4: PFP = [Parallel Functional Programming](http://www.cse.chalmers.se/edu/course/DAT280_Parallel_Functional_Programming/), Mary Sheeran & John Hughes
    * LP4: Comp = [Compiler Construction](http://www.cse.chalmers.se/edu/course/TDA283/), Josef Svenningsson -> Thomas Hallgren
    * ...


Forskning
================

* QuickCheck
*

Företag
================

* [Functional Jobs](http://functionaljobs.com/)
* Facebook, Google, Standard Chartered, Microsoft Research, Klarna, ...
* Galois, FP-Complete, WellTyped, Quviq, ...


Historia
================

Exempel:

* Fortsättningskursen "Avancerad funktionell programmering" har getts varje år sedan 1993:
    * 1993: John Hughes, 1995: Magnus Carlsson, 1999: Lennart Augustsson, 2000: Koen Claessen, 2003: Josef Svenningsson, 2008: Ulf Norell, 2010 och framåt: Patrik Jansson

Matematikens domänspecifika språk
================

* Ny kurs i LP3 2016 för Chalmers och GU.
* Valfri för D, DV, IT, TM, ...

* Examinator: Patrik Jansson
* Föreläsare: Cezar Ionescu

Förkunskaper: "ett års studier"
* en kurs i diskret matematik (pågår)
* två andra kurser i matematik (exampelvis Linjär algebra och Analys)
* två kurser i datateknik (exemeplvis två kurser i programmering)
* ytterligare tre kurser (22.5hp) inom matematik, data eller IT


Kursen presenterar klassiska matematiska ämnen från ett
datavetenskaligt perspektiv: genom att specificera de introducerade
begreppen, vara uppmärksam på syntax och typer, och slutligen genom
att bygga domänspecifika språk för vissa av de matematiska områden som
nämns nedan.

DSLsofMath: Lärandemål
================

* designa och implementera ett domänspecifikt språk (DSL) för en ny domän
* strukturera delområden inom matematik i termer av DSL
* förklara de centrala begreppen i grudläggande reell och complex anays, algebra och linjär algebra

* utveckla lämplig notation för matematiska koncept
* genomföra och kalkylera bevis
* använda potensserier för att lösa differentialekvationer
* använda Laplace-transformer för att lösa differentialekvationer

* diskutera och jämföra olika implementationer av matematiska begrepp

Föreläsningarna kommer att behandla:
* Introduktion till funktionell programmering, programkalkyl och bevis
* Introduktion till domänspecifika språk (DSL) med linjär algebra som exempel
* DSL och matematik: kategoriteori som exempel
* Reell analys: medelvärdessatser, Taylors formuler
* Reell analys: ett DSL för potensserier
* Mer linjär algebra: egenvärden och optimering

DSLsofMath: motivation
================

Programmering och matematik är väldigt närliggande ämnen
