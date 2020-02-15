import "io"

manifest
{
  left = 0,
  data = 1,
  right = 2,
  size = 1000,
  sizeof_node = 3
}
  
let new_node(x) be
{
  let ptr = newvec(sizeof_node);
  ptr ! data  : = x;
  ptr ! left  : = nil;
  ptr ! right : = nil
  resultis ptr;
}  

let add(key,val) be
{

}

let remove(key) be
{

}

let buildTree(root)
{

}

let printTree(tree)
{

}

let 

let start() be
{

  /* 
   *  Program must:
   *  - accept string from user
   *  - put them into a binary tree
   *  - print tree when user types * in alphabetical order
   *  - delete entrie tree, and repat all over
   */
  let uInput, tree;

  uInput := getInput();

  while true do
  {
    uInput := getInput();
    tree := buildTree(uInput);
    if(uInput = '*')
    {
      printTree(tree);
      rmTree(tree);
      uInput = getInput();
      tree := buildTree(uInput);
    }
  }

}
