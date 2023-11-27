open Graph


type label =
  { value: int ;
    max: int }

val dfs: label graph -> id -> id -> label arc list