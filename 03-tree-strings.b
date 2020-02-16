import "io"

manifest
{
  data = 0,
  left = 1,
  right = 2,
  size = 10000,
  sizeof_node = 3,
  buff = 2
}
  
let new_node(x) be
{
  let ptr = newvec(sizeof_node);
  ptr ! data  := x;
  ptr ! left  := nil;
  ptr ! right := nil;
  resultis ptr;
}  


/* adds an element to the tree */
/* root: the root node of the tree, is a pointer to a pointer*/
let add(root, val) be
{
  let node;

  test root = nil then
  {
    node := new_node(val);
    resultis node;
  }
  else test root!data > val then
  {
    root!left := add(root!left,node);
  }
  else
  {
    root!right := add(root!right,node);
  }

  resultis root;
}

/*[> removes an element from the tree <]*/
/*let remove(val) be*/
/*{*/

/*}*/

/*[> builds a new tree with 'root' as the root element <]*/
/*let buildTree(root)*/
/*{*/

/*}*/

/* delete entire tree */
/*let rmTree(root)*/
/*{*/

/*}*/

/*[> print tree <]*/
/*let printTree(root)*/
/*{*/
  /*test root = nil then*/

/*}*/

let strcpy(dst, src) be
{
  let i = 0;
  while true do
  {
    let c = byte i of src;
    if c = 0 then
    {
      byte i of dst := c;
      break;
    }
    byte i of dst := c;
    i +:= 1
  }
}

/* src is a ptr to the str being copied
   size is a pointer to the size of the 
   string being coped
*/
let resizeStr(src,size) be
{
  let dst; 
  !size *:= 2; 
  dst := newvec(!size);
  strcpy(dst,!src);
  resultis dst;
}

let getInput() be
{
   let c, size = buff, str = newvec(size), length = 0;
   while true do
   {
     test length < size do
     {
       c := inch();
       if c < 'A' \/ c > 'z' then
         break;
       byte length of str:=c;
       length +:= 1;
     }
     else
     {
       str := resizeStr(@str,@size);
     }
   }
    byte length+1 of str:=0;
    resultis str;
}


let start() be
{

  /*  TODO:
   *  Program must:
   *  - accept string from user
   *  - put them into a binary tree
   *  - print tree in alphabetical order when user types * 
   *  - delete entrie tree, and repat all over
   */

  let uInput, treeRoot, heap = vec(size), i = 0;

  /* initialize heap */
  init(heap,size);

  uInput := getInput();
  out("string entered: %s\n",uInput);
  while true do
  {
    test byte i of uInput ~= 0 then
    {
      out("%c",byte i of uInput);
    }
    else
    {
      break;
    }
  } 
  //treeRoot := buildTree(uInput);
/*
  while true do
  {
    uInput := getInput();
    if(uInput = '*')
    {
      printTree(treeRoot);
      rmTree(treeRoot);
      out("user input: %s\n",uInput);
    }
    else
    {
      add(treeRoot,uInput);
    }
  }
*/

}
