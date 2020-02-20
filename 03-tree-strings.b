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
  INVALID_CHAR = -1 //const used to indicate skip char
}

/*
static { vecsize = 0, vecused = 0, vecspace }

let init(v, s) be 
{ 
  vecsize := s;
  vecspace := v; vecused := 0 
}

let lamest_newvec(n) be
{

}

let lamest_freevec(v) be 
{

}
*/
  
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
          break;
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
    i +:= 1 
  }
}

/* src is a ptr to the str being copied
   size is a pointer to the size of the 
   string being copied
*/
let resizeStr(str,len) be
{
  let newstr;
  !len *:= 2; //double size of str
  newstr := newvec(!len); //allocate memory for new str
  for i = 0 to !len - 1 do //zero out memory of new str
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
      max_size = buff, 
      str = newvec(max_size), 
      length = 0, 
      bytes_per_word = 4,
      current_letter;

  while true do
  {
     char := inch(); //get char from user.

     if length = 0 /\ isBlank(char) = true do //check if first char is blank.
     {
        str:= INVALID_CHAR; //if so return invalid char error code.
        break;
     }
      
     test length < max_size * bytes_per_word do //check if str length < max size of str
     {                                              //converted to bytes (-1 leaving room for string terminator).
         if length = 0 /\ char = '*' do //if first char is * return print to print tree.
         {
             str:= PRINT; //return print
             break;
         }

         test validate(char) = false do 
         {                              
            test isBlank(char) = false do
            {
              byte length of str := 0;
            }
            else
            {
              break;
            }
         }
         else //otherwise keep building string by appending chars
         {
             byte length of str:= char;
             length +:= 1;
         }
     }
     else //resize string
     {
         str := resizeStr(@str,@max_size);
     }
  }

  resultis str; //return str
}


let start() be
{

  /*  TODO:
   *  Program must:
   *  - accept string from user (done)
   *  - put them into a binary tree (done)
   *  - print tree in alphabetical order when user types (done) 
   *  - delete entrie tree, and repat all over (done)
   */

  let uInput = nil, treeRoot = nil, heap = vec(size);
  /* initialize heap */
  init(heap,size);

  while true do
  {
    uInput := getInput();
    out("str=%s,c=%c,hex=%x\n",uInput,uInput,uInput);
    switchon uInput into
    {
      case PRINT:
        out("printing tree\n");
        printTree(treeRoot);
        rmTree(@treeRoot); //pass the tree by reference so I can remove all nodes
        treeRoot := nil;
        endcase;
      case INVALID_CHAR: //do nothing (i.e loop if input invalid)
        endcase;
      default:
        out("adding %s to tree\n",uInput);
        treeRoot := add(treeRoot,uInput);
    }

  }

}
