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

static { vecsize = 0, vecused = 0, vecspace, freelist}

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
  out("freelist[%d]=%d\n",freelist!ch_size-1,freelist!(freelist!ch_size-1));


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
 // out("%d is the requested size, %d is the adjusted size\n",n,adj_size);

  curr_chunk := head;
  //search for freeblock that causedn fit request needs
  while curr_chunk /= nil do
  {
    curr_chunk := head;
    if adj_size <= curr_chunk!ch_size do
    {
        test adj_size < curr_chunk!ch_size do //split the block
        {
            used_chunk := curr_chunk + (curr_chunk!ch_size) - adj_size;
            used_chunk!-1 := (curr_chunk!ch_size)-adj_size;
            used_chunk!ch_size := adj_size;
            used_chunk!flag := in_use;
            used_chunk!(adj_size-1) := adj_size;
            curr_chunk!ch_size -:= adj_size;

            vecused +:= adj_size;
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
            else test curr_chunk!prev = nil do
            {//head of list
              (used_chunk!next)!prev := nil;
              freelist := used_chunk!next;
            }
            else test curr_chunk!next = nil do
            {//tail of list
              (used_chunk!prev)!next := nil;
            }
            else
            {
              (used_chunk!next)!prev := used_chunk!prev; 
              (used_chunk!prev)!next := used_chunk!next;
            }

            vecused +:= adj_size;
            resultis used_chunk + 2;
        }   
    }
    curr_chunk := curr_chunk!next;
  }

out("no block found: insuffcient memory\n");
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

      //out("in getInput\n");
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

  let uInput = nil, treeRoot = nil, heap = vec(size), array1, array2, array3;
  newvec := my_newvec; /*freevec = my_freevec,*/ init := my_init; 
  /* initialize heap */
  init(heap,size);
  //array1 := newvec(5);//testing newvec with small chunks
  //array2 := newvec(13);//testing newvec with small chunks
  /*array3 := newvec(size);//testing newvec with size that exceeds capacity*/

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
