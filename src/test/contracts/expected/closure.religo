let test = 
  (k: int): int => 
    {
      let j: int = k + 5;
      let close: (int => int) = (i: int) => i + j;
      let j: int = 20;
      close(20)
    };