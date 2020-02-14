import "io"

manifest
{
    next_node = 0,
    data = 1, 
    size = 10000,
    sizeof_node = 2
}

let new_node(x) be
{
    let ptr = newvec(sizeof_node);
    ptr ! data := x;
    ptr ! next_node := nil;
    resultis ptr;
}

let start() be
{
    let heap = vec(size), input = 0, head, node;
    
    /* initialize heap */
    init(heap,size);

    out("Enter postive integers to add to list, enter negative integer to exit\n");
    input := inno();

    /* create new node */
    node := new_node(input);

    /* set head to newly created node */
    head := node;

    /* while input is not negative 
     * create new node and append it to list
     */
    until input < 0 do
    {
        input := inno();
        node ! next_node := new_node(input);
        node := node ! next_node; 
    }

    out("printing list\n");
    out("----------------------\n");

    /* while list is not empty
     * print out element in list
     */
    until head ! next_node = nil do
    {
        out("%d\n",head ! data);
        head := head ! next_node;
        freevec(head);
    }
    freevec(node);

    
}
