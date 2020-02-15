import "io"

manifest
{
  data = 0,
  left = 1,
  right = 2,
  size = 1000,
  sizeof_node = 3,
  buff = 20
}
  
let new_node(x) be
{
  let ptr = newvec(sizeof_node);
  ptr ! data  : = x;
  ptr ! left  : = nil;
  ptr ! right : = nil
  resultis ptr;
}  


/* adds an element to the tree */
/* root: the root node of the tree, is a pointer to a pointer*/
let add(root, val) be
{
  let @ node;
  node = new_node(val);
  test !root = nil then
  {
    resultis node;
  }
  else test !root!data > val then
  {
    !root!left := add(!root!left,node);
  }
  else
  {
    !root!right := add(!root!right,node);
  }

  resultis root;
}

/* removes an element from the tree */
let remove(val) be
{

}

/* builds a new tree with 'root' as the root element */
let buildTree(root)
{

}

/* delete entire tree */
let rmTree(root)
{

}

/* print tree */
let printTree(root)
{
  test root = nil then

}

let getInput()
{
   let str;

   until
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

  let uInput, treeRoot, heap = vec(size);

  /* initialize heap */
  init(heap,size);

  uInput := getInput();
  treeRoot := buildTree(uInput);

  while true do
  {
    uInput := getInput();
    if(uInput = '*')
    {
      printTree(treeRoot);
      rmTree(treeRoot);
    }
    else
    {
      add(treeRoot,uInput);
    }
  }

}
