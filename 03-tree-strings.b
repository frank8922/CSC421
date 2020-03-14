import "io"

manifest
{
  data = 0,
  left = 1,
  right = 2,
  size = 10000,
  sizeof_node = 3,
  buff = 2, //in words (i.e 32bit words)
  PRINT = -2, //const used to indicate to print tree 
  INVALID_CHAR = -1
}


manifest
{

  ch_size = 0,
  flag = 1,
  next = 2,
  prev = 3,
  in_use = 98989898,
  free = 12121212

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
      usedblk_size = adj_size;

      heap := vecspace;
      heap_bot := heap + size-1;
      lastblock := heap_bot - !heap_bot+1;

  //if the size requseted (incld pointers) is < the heap, no memory
  if vecsize < adj_size /\ vecused >= vecsize do 
  { out("insufficent memory\n"); return; }

  //link the blocks in the freelist
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

      
   curr_chunk := freelist;
  //search for freeblock that fit request needs
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
       usedblock,
       merged_above = false,
       merged_below = false;
         
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
    nextblock := usedblock; //set the reference block below
    block_above := usedblock; //set the reference block above

    nextblock +:= usedblock!ch_size; //calc block below
    block_above := block_above - usedblock!-1; //calc block above
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



  
let new_node(x) be
{
  let ptr = newvec(sizeof_node);
  ptr ! data  := x;
  ptr ! left  := nil;
  ptr ! right := nil;
  resultis ptr;
}

/* helper function to compare strings */
let strcmp(x,y) be
{
  let i = 0, val;
  while true do
  {
    let char_x = byte i of x,
        char_y = byte i of y;

        test char_x < char_y do
        {
          val := -1;
          break;
        }
        else test char_x = char_y do
        {
          val := 0;
        }
        else
        {
          val := 1;
          break;
        }
        
        x +:= 1;
  }
  resultis val;
}

/* adds an element to the tree */
/* root: the root node of the tree, is a pointer to a pointer*/
let add(root, val) be
{
  test root = nil then
  {
    root := new_node(val);
  }
  else test strcmp(root ! data,val) > 0 then
  {
    root ! left := add(root ! left,val);
  }
  else
  {
    root ! right := add(root ! right,val);
  }

  resultis root;
}


/* delete entire tree */
let rmTree(rootPtr) be
{
  if rootPtr = nil then
  {
    return; 
  }
  freevec(rootPtr!left);
  rootPtr!left := nil;
  freevec(rootPtr!right);
  rootPtr ! right := nil;
  freevec(rootPtr!data);
  rootPtr!data := nil;
  freevec(rootPtr);
  rootPtr := nil;
}

/* print tree */
let printTree(rootPtr) be
{
  if rootPtr = nil then
  {
    return; 
  }
  printTree(rootPtr ! left);
  out("printing %s\n",rootPtr!data);
  printTree(rootPtr ! right);

}

/* helper function to copy string */
let strcpy(dst, src) be
{
  let i = 0;
  while true do
  {
    let char = byte i of src; //current letter in src[i]

    if char = 0 then //if src[i] == '\0'
    {
      byte i of dst := char; //dst[i] = '\0'
      break;
    }
    byte i of dst := char; //dst[i] += src[i]
    i +:= 1; 
  }
}

/* src is a ptr to the str being copied
   size is a pointer to the size of the 
   string being copied
*/
let resizeStr(str,len) be
{
  let newstr, sz = len;
  sz *:= 2; //double size of str
  newstr := newvec(sz); //allocate memory for new str
  for i = 0 to sz - 1 do //zero out memory of new str
  {
    newstr ! i := 0;
  }
  strcpy(newstr,str); //copy old str contents to new str
  //freevec(!str); //free old str
  freevec(str);
  str := nil; //set pointer to null
  resultis newstr; //return new str
}

/* helper function to validate char */
let validate(x) be
{
  let val;

  switchon x into
  {
    case 'A'...'Z':
      val := true;
      endcase;
    case 'a'...'z':
      val := true;
      endcase;
    default:
      val := false;
      endcase;
  }

  resultis val;
}

/* helper function to detect blanks */
let isBlank(x) be
{
  let val;
  switchon x into
  {
    case 0...32: //ascii values we don't care about (space,tabs,etc)
      val:= true;
      endcase;
    default:
     val := false;
     endcase;
  }
  resultis val;
}


/* if '*' is the first char, returns print code 
   if first char is blank returns invalid char code
   otherwise returns string from stdin. 
*/
let getInput() be
{
   /* local variables */
  let char, 
      max_size = 2, 
      str, //= newvec(max_size), 
      length = 0, 
      bytes_per_word = 4;

//check if first char is valid,if valid then add to str
//otherwise do not allocate string until user enters valid char
 while true do
 {
     char := inch(); //get char from user.
     test validate(char) = true do
     {
        str := newvec(max_size);
        byte length of str := char;
        length +:= 1;
        break;
     }
     else test char = '*' do
     {
        resultis PRINT;
     }
     else
     {
        resultis INVALID_CHAR;
     }
  }

  while true do
  {
     char := inch(); //get char from user.
     test validate(char) = true do
     {
        test length < (max_size * bytes_per_word - 1) do
        {
          byte length of str:= char;
        }
        else
        { 
          str :=resizeStr(str,max_size);
          max_size *:= 2;
          byte length of str:= char;
        }
        length +:= 1;
     }
     else 
     {
        byte length of str := 0;
        break;
     }

 }

  //out("%x=%s\n",str,str);
  resultis str; //return str
}


let start() be
{

  let uInput = nil, treeRoot = nil, heap = vec(size), array1, array2, array3;
  newvec := my_newvec; freevec := my_freevec; init := my_init; 
  /* initialize heap */
  init(heap,size);

  while true do
  {
    uInput := getInput();
    switchon uInput into
    {
      case PRINT:
        out("printing tree\n");
        printTree(treeRoot);
        rmTree(treeRoot); //pass the tree by reference so I can remove all nodes
        treeRoot := nil;
        //freevec(uInput);
        //printHeap();
        //printFreelist();
        endcase;
      case INVALID_CHAR:
          freevec(uInput);
          endcase;
      default:
        out("adding %s to tree\n",uInput);
        treeRoot := add(treeRoot,uInput);
    }

  }

}
