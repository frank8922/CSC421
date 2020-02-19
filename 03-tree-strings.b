import "io"

manifest
{
  data = 0,
  left = 1,
  right = 2,
  size = 10000,
  sizeof_node = 3,
  buff = 2, //in words (i.e 32bit words)
  PRINT = 2, 
  INVALID = -1
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
  let i = 0;
  while true do
  {
    let char_x = byte i of x,
        char_y = byte i of y;

        test char_x < char_y do
        {
          resultis -1;
        }
        else test char_x = char_y do
        {
          resultis 0;
        }
        else
        {
          resultis 1;
        }
        
        x +:= 1;
  }
}

/* adds an element to the tree */
/* root: the root node of the tree, is a pointer to a pointer*/
let add(root, val) be
{
  if root = nil then
  {
    resultis new_node(val);
  }
  test strcmp(root ! data,val) > 0 then
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
  test x < 'A' \/ x > 'z' do
  {
    resultis false;
  }
  else
  {
    resultis true;
  }
}

/* helper function to detect newline */
let isNewline(x) be
{
  resultis x = '\n'
}


let getInput() be
{
   /* local variables */
  let char, max_size = buff, str = newvec(max_size), length = 0, bytes_per_word = 4;
  
  while true do
  {
     char := inch(); //get char from user
   
      //check if first char is newline
     if length = 0 /\ isNewLine(char) = true do
     {
       break //if so get a new char
     }

      //check if string length less than 
      //max size converted to bytes (-1 leaving room for string terminator)
     test length < max_size * bytes_per_word - 1 do
     {
         //if first char is * return print to print tree
         test length = 0 /\ char = '*' do
         {
           resultis PRINT;
         }
         //if not a valid char or is a newline, in the middle of word, 
         //then return string with string terminator appended
         else test validate(char) = false \/ isNewline(char) = true do
         {
             //check if past the first char, 
             //if so place string terminator on existing word and return string
             test length > 0 do
             {
               byte length of str := 0;
               resultis str;
             }
             else //otherwise return invalid (meaning request input again)
             {
               resultis INVALID; 
             }
         }
         else //otherwise keep building string by appending chars
         {
           byte length of str := char;
           length +:= 1;
         }
     }
     else //resize string
     {
       str := resizeStr(@str,@max_size);
     }

  }

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
    switchon uInput into
    {
      case PRINT:
        out("printing tree\n");
        printTree(treeRoot);

        //pass the tree by reference so I can remove all nodes
        rmTree(@treeRoot);
        endcase;
      case INVALID: //do nothing (i.e loop if input invalid)
        endcase;
      default:
        out("adding %s to tree\n",uInput);
        treeRoot := add(treeRoot,uInput);
    }
  }

}
