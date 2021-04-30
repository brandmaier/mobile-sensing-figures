require("DiagrammeR")

grViz("
      digraph {
      Drinking
      Walking
      Stairs
      Folding
      
      Walking->Stairs
      Stairs->Walking
      Walking->Drinking
      Drinking->Walking
      Walking->Folding
      Folding->Walking
      Drinking->Drinking
      Stairs->Stairs
      Folding->Folding
      Folding->Drinking
      Drinking->Folding
      }
      ")