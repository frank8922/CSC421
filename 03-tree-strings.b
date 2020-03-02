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



static { vecsize = 0, vecused = 0, vecspace, freelist}

let my_init(v, s) be 
{ 
  let initial_size;

  manifest
  {
    chunk_size = 0,
    flag = 1, 
    not_in_use = 12121212,
    in_use = 98989898,
    prev_freeblk = 2,
    next_freeblk = 3
  }

  vecsize := s;
  vecspace := v; 
  freelist := vecspace;

  freelist ! chunk_size := vecsize;
  freelist ! flag := not_in_use;
  freelist ! prev_freeblk := -1;
  freelist ! next_freeblk := -1;
  freelist ! size := vecsize;
  
  out("**init()**\nfreelist[chunksize]=%d address=%x\nfreelist[flag]=%d address=%x\nfreelist[prev_freeblk]=%d address=%x\nfreelist[next_freeblk]=%d address=%x\nfreelist[chunksize]=%d address=%x\n**end init**\n",

  freelist ! chunk_size,@(freelist ! chunk_size),
  freelist ! flag,@(freelist ! flag),
  freelist ! prev_freeblk,@(freelist ! prev_freeblk),
  freelist ! next_freeblk,@(freelist ! next_freeblk),
  freelist ! size,@(freelist ! size));

}

let my_newvec(n) be
{
  let head_freelist = freelist, new_size = 0, heap_size = vecsize - vecused, 
      current_freeblk, old_size, found = false,
      usedblk, current_blk_size;

  manifest
  {
    chunk_size = 0,
    flag = 1, 
    not_in_use = 12121212,
    in_use = 98989898,
    prev_freeblk = 2,
    next_freeblk = 3
  }

  /*new_chunk_size := n+(8-(n rem 8)); //calculate new size (min of 5 ptrs)*/

  new_size := n + 3; //add 3 to account for pointers
  current_freeblk := head_freelist; //start head of freelist
  current_blk_size := (current_freeblk ! chunk_size); //freeblock size

  out("\n**mynewvec()**\ncurrent_freeblk_size=%d\n",current_blk_size); //size before splitting block
  out("requested_chunk_size=%d",new_size); //size of new used chunk

  //check if space available in heap
  if heap_size > new_size do 
  {
    while true do
    {
      //check if the current free block can fit requested block
      test current_freeblk ! chunk_size >= new_size do  
      {
          //get old size
          old_size := current_freeblk ! chunk_size;
          
          //adjust size of freeblk at begining and end of block
          current_freeblk ! old_size := old_size - new_size;
          current_freeblk ! chunk_size := old_size - new_size;

          //set address of used block
          usedblk := @(current_freeblk ! current_blk_size)-new_size;

          //set size of used chunk
          usedblk ! chunk_size := new_size;

          //set in use flag
          usedblk ! flag := in_use;

          vecused +:= new_size;



out("\ncurrent_freeblk size=%d from address=%x to address=%x\nusedblk size=%d flag=%d from address=%x to address=%x\n",
       current_freeblk ! chunk_size, @(!current_freeblk), @(!usedblk)-1,
       usedblk ! chunk_size, usedblk ! flag, @(!usedblk),@(!usedblk) + usedblk!chunk_size);




          out("returned a block\n"); 
          out("**end mynewvec()**\n");
          resultis usedblk;
      }
      else
      {
          test current_freeblk ! next_freeblk = -1 do
          {
            break;
          }
          else
          {
            current_freeblk := current_freeblk ! next_freeblk;
          }
      }
    }
  }
  
    //else no memory
    out("\ninsufficent free memory\n");
    out("**end mynewvec()**\n");
    finish;
}


let my_freevec(v) be 
{
  out("in freevec");

}



  
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
  freevec(!rootPtr ! left);
  !rootPtr ! left := nil;
  freevec(!rootPtr ! right);
  !rootPtr ! right := nil;
  freevec(!rootPtr);
  !rootPtr := nil;
}

/* print tree */
let printTree(rootPtr) be
{
  if rootPtr = nil then
  {
    return; 
  }
  printTree(rootPtr ! left);
  out("%s\n",rootPtr ! data);
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
  strcpy(newstr,!str); //copy old str contents to new str
  freevec(!str); //free old str
  !str := nil; //set pointer to null
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
      str = newvec(max_size), 
      length = 0, 
      bytes_per_word = 4;

      out("in getInput\n");
  while true do
  {
     char := inch(); //get char from user.
     test validate(char) = true do
     {
        test length < max_size * bytes_per_word - 1 do
        {
          byte length of str:= char;
        }
        else
        { 
          str:=resizeStr(@str,max_size);
          max_size *:= 2;
          byte length of str:= char;
        }
        length +:= 1;
     }
     else 
     {
       test length = 0 do
       {
         test char = '*' do
         {
            freevec(str);
            str:=PRINT;
            break;
         }
         else
         {
            freevec(str);
            str:= INVALID_CHAR;
            break;
         }
       }
       else
       {
          byte length of str := 0;
          break;
       }
    }

 }

  resultis str; //return str
}


let start() be
{

  let uInput = nil, treeRoot = nil, heap = vec(size), array1, array2, array3,
  newvec = my_newvec, freevec = my_freevec, init = my_init; 
  /* initialize heap */
  init(heap,size);
  array1 := newvec(5);//testing newvec
  array2 := newvec(13);//testing newvec
  array3 := newvec(size);//testing newvec

  while true do
  {
    uInput := getInput();
    switchon uInput into
    {
      case PRINT:
        out("printing tree\n");
        printTree(treeRoot);
        rmTree(@treeRoot); //pass the tree by reference so I can remove all nodes
        treeRoot := nil;
        endcase;
      case INVALID_CHAR:
          endcase;
      default:
        out("adding %s to tree\n",uInput);
        treeRoot := add(treeRoot,uInput);
    }

  }

}
