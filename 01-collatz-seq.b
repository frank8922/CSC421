import "io"

let collatz_seq() be
{
   let N, x, count;

   out("Enter a number (N):\n");
   N := inno(); 
   x := N;
   count := 0;

   out("\n%d,",N);

   while x > 1 do
   {
       test x rem 2 = 0 then
        x := x/2
       else
        x := x * 3 + 1;
        
        out("%d, ",x);
        count +:= 1;
   }
    out("\n length: %d\n",count);
}


let start() be
{
    collatz_seq() repeat;
}