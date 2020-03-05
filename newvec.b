import "io"

manifest
{

  ch_size = 0,
  flag = 1,
  next = 2,
  prev = 3,
  in_use = 98989898,
  free = 12121212

}

static { vecused = 0, vecspace, vecsize = 0, freelist }


let my_init(v,s) be
{
  
  vecspace := v;
  vecsize := s;
  freelist := vecspace;

  freelist ! ch_size := s;
  out("freelist[%d]=%d address=%x\n",ch_size,freelist!ch_size,freelist);
  freelist ! prev := nil;
  out("freelist[%d]=%d\n",prev,freelist!prev);
  freelist ! next := nil;
  out("freelist[%d]=%d\n",next,freelist!next);
  freelist ! flag := free;
  out("freelist[%d]=%d\n",flag,freelist!flag);
  //set end of chunk
  freelist!(freelist!ch_size-1) := freelist!ch_size;
  out("freelist[%d]=%d\n",freelist!ch_size,freelist!(freelist!ch_size));


}


let my_newvec(n) be
{

  let offset = n + 3, 
      used_chunk, 
      curr_chunk, 
      req_size     = n,
      adj_size     = 5 + offset - (offset rem 5),
      usedblk_size = adj_size,  
      head = freelist;

  //if the size requseted (incld pointers) is < the heap, no memory
  if vecsize < adj_size do { out("insufficent memory\n"); finish; }
  out("%d is the requested size, %d is the adjusted size\n",n,adj_size);

  curr_chunk := head;
  //search for freeblock that causedn fit request needs
  while curr_chunk /= nil do
  {
    curr_chunk := head;
    if adj_size <= curr_chunk!ch_size do
    {
        test adj_size < curr_chunk!ch_size do //split the block
        {
            used_chunk := !(curr_chunk!ch_size)-adj_size;
            out("curr_chunk[%d]=%d, addr=%x\n",(curr_chunk!ch_size)-adj_size,(curr_chunk!ch_size)-adj_size,used_chunk);
            used_chunk!ch_size := adj_size;
            out("used_chunk[%d]=%d, addr=%x\n",ch_size,used_chunk!ch_size,used_chunk);
            used_chunk!flag := in_use;
            used_chunk!(ch_size+adj_size-1):= adj_size;
            out("used_chunk[%d]=%d, addr=%x\n",ch_size+adj_size-1,(used_chunk!ch_size)+adj_size-1,@(used_chunk!ch_size)+adj_size-1);

            vecused +:= adj_size;
            resultis used_chunk!(flag+1);
        }
        else // otherwise n equal to chunk_size; use whole block
        {
            used_chunk := curr_chunk;
            used_chunk!ch_size := curr_chunk!ch_size;
            used_chunk!((used_chunk!ch_size)-1) := used_chunk!ch_size;
            used_chunk!flag := in_use;

           test curr_chunk!prev = nil /\ curr_chunk!next = nil do //then its the last free chunk
            { //swap pointers
              used_chunk!next!prev := used_chunk!prev; 
              used_chunk!prev!next := used_chunk!next;
            }
            else test curr_chunk!prev = nil do
            {//head of list
              used_chunk!next!prev := nil;
              used_chunk!next := nil;
            }
            else
            {//tail of list
              used_chunk!prev!next := nil;
              used_chunk!prev := nil;
            }

            vecused +:= adj_size;
            resultis used_chunk!(flag+1);
        }   
    }
    curr_chunk := curr_chunk!next;
  }

out("no block found: insuffcient memory\n");
finish;
}



/*let my_freevec(n)
{

}*/


let start() be
{ 
  let array, array2, v = vec(100), s = 100,
  newvec = my_newvec, init = my_init; //freevec = my_freevec;
  my_init(v,s);

  array := newvec(10);
  array2 := newvec(70);
  for i = 0 to 10 do
  {
    array!i := i;
  }

  for i = 0 to 10 do 
  {
    out("%d is element an in array\n",i);
    //out("%x\n",!(array!i));
  }

  for i = 0 to 70 do
  {
    array2!i := i;
  }

  for i = 0 to 70 do 
  {
    out("%d is element an in array2\n",i);
    //out("%x\n",!(array2!i));
  }
}
