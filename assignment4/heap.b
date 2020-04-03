import "io"

export {vecspace,vecsize,my_init,my_freevec,my_newvec}

manifest
{

  ch_size = 0,
  flag = 1,
  next = 2,
  prev = 3,
  in_use = 98989898,
  free = 12121212,
  size = 10000
}

static { vecsize = 0, vecused = 0, vecspace, freelist, firstfree} 

let printHeap() be 
{
  out("ADDRESS INT STRG\n");
  for i = 0 to size-1 do
  {
    out("%x  %d  %s\n",@(vecspace!i),vecspace!i,vecspace!i);
  }
}

let printFreelist() be
{
   let head = freelist;
   out("PRINTING FREELIST\n");
   while head /= nil do
   {
      out("%x %d\n",head,head!ch_size);
      head := head!next;
   }

}


let my_init(v,s) be
{
  
  vecspace := v;
  vecsize := s;
  freelist := vecspace;

  freelist ! ch_size := s;
  freelist ! prev := nil;
  freelist ! next := nil;
  freelist ! flag := free;
  //set end of chunk
  freelist!(freelist!ch_size-1) := freelist!ch_size;
  firstfree := freelist;

  //printHeap();

}

let my_newvec(n) be
{

  let offset = n + 3, 
      used_chunk, 
      curr_chunk, 
      lastblock, 
      firstblock,
      heap,
      heap_bot,
      req_size     = n,
      adj_size     = 5 + offset - (offset rem 5), //smallest possible chunk 10
      usedblk_size = adj_size,  
      head = freelist;

      heap := vecspace;
      heap_bot := heap + size-1;
      lastblock := heap_bot - !heap_bot+1;
  //out("user requested size %d, user adj size %d\n",req_size,adj_size);
  //if the size requseted (incld pointers) is < the heap, no memory
  if vecsize < adj_size /\ vecused >= vecsize do 
  { out("insufficent memory\n"); return; }

   firstblock := heap;
   while firstblock < heap_bot do
   {
     if firstblock!flag = free do 
     {
       freelist!next := firstblock;
       firstblock!prev := freelist;
       freelist := freelist!next;
     }
     firstblock +:= firstblock!ch_size;
   }
   freelist!next := nil;
   freelist := firstfree;

      
    curr_chunk := head;
  //search for freeblock that causedn fit request needs
  while curr_chunk /= nil do
  {
    if adj_size <= curr_chunk!ch_size do
    {
        test adj_size < curr_chunk!ch_size do //split the block
        {
            /*userchunk =  the current chunk size - user adjusted size */
            used_chunk := curr_chunk + (curr_chunk!ch_size) - adj_size;

            /*bottom of currentchunk = usedchunk-1*/
            used_chunk!-1 := (curr_chunk!ch_size)-adj_size;

            /*set the usedchunks size to user adjusted size*/
            used_chunk!ch_size := adj_size;

            /*set flags*/
            used_chunk!flag := in_use;

            /*set the bottom of usedchunk to proper size */
            used_chunk!(adj_size-1) := adj_size;

            /*subtract the user adjusted size from top of current chunk*/
            curr_chunk!ch_size -:= adj_size;

            vecused +:= used_chunk!ch_size;
            /* return block for user to use */
            resultis used_chunk + 2;
        }
        else // otherwise n equal to chunk_size; use whole block
        {
           used_chunk := curr_chunk;
           used_chunk!flag := in_use;

           test curr_chunk!prev = nil /\ curr_chunk!next = nil do //then its the last free chunk
            { //swap pointers
              freelist := nil;
            }
            else test curr_chunk!prev = nil do//check if curr_chunk is head of list
            {
              (used_chunk!next)!prev := nil;
              freelist := used_chunk!next;
            }
            else test curr_chunk!next = nil do//check if curr_chunk tail of list
            {
              (used_chunk!prev)!next := nil;
            }
            else
            {
              (used_chunk!next)!prev := used_chunk!prev; 
              (used_chunk!prev)!next := used_chunk!next;
            }

            vecused +:= used_chunk!ch_size;
            resultis used_chunk + 2;
        }   
    }
    curr_chunk := curr_chunk!next;
  }

out("no block found: insuffcient memory\n");
return;
}//end newvec


let my_freevec(p) be 
{
   let header = -2,lastblock,diff,
       heap,heap_bot,firstblock,block_above,
       nextblock,head,
       usedblock,merged_above = false,merged_below = false;
         
    //get heap bounderies
    heap := vecspace;
    heap_bot := heap + size-1;

    if heap_bot - (p+header) < 0 do
    { /*out("%x out of bounds\n",p+header);*/ return;}

    if heap_bot - (p+header) > size-1 do
    { /*out("%x out of bounds\n",p+header);*/ return;}
    
    //get header
    usedblock := p + header;

    //set flag to free
    usedblock !flag := free;
    
    vecused -:= usedblock!ch_size;

          
    head := heap;
    nextblock := head + (head!ch_size);
    nextblock := usedblock; //set the reference block
    block_above := usedblock; //set the reference block

    nextblock +:= usedblock!ch_size; //calc nextblock
    block_above := block_above - usedblock!-1; //calc nextblock*/
    lastblock := heap_bot - !heap_bot+1;
    

    //block below is free
    if nextblock!flag = free /\ nextblock <= lastblock do
    {
       usedblock!ch_size +:= nextblock!ch_size;  
       !(nextblock + nextblock!ch_size -1) := usedblock!ch_size;
       merged_below := true;

    }
    //block above is free
    if block_above!flag = free /\ block_above >= heap do 
    {
       !(usedblock + usedblock!ch_size-1) +:= block_above!ch_size;
       block_above!ch_size +:= usedblock!ch_size;
       merged_above := true;
    }
    
    if merged_above = false do
    {
        freelist!next := usedblock;
        usedblock!prev := freelist;
    }

    if merged_above = true do
    {
       freelist!next := block_above;
       block_above!prev := freelist;
    }
    freelist := freelist!next;

    //search for first free chunk
    for i = 0 to size-1 do
    {
      if heap!i = free do
      {
        freelist := heap + (i - 1);
        firstfree := freelist;
        break;
      }
    }

 freelist := firstfree;
}//end of freevec()
