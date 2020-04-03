import "io"

let make_heap() be 
             // uses free memory after end of loaded program
             // rounds beginning address up to a multiple of 1K
{ let begin = (((! 0x101) >> 10) + 1) << 10, end = ! 0x100;
  init(begin, end - begin); }

let str_eq(a, b) be
             // two ordinary zero terminated strings
{ let i = 0;
  while true do
  { let ca = byte i of a, cb = byte i of b;
    if ca <> cb then 
      resultis false;
    if ca = 0 then
      resultis true;
    i +:= 1 } }

let string_vtof(s, fs, chars) be
             // v to f: variable to fixed
             // s is ordinary zero terminated string, any length
             // fs is a fixed length string (chars = bytes available),
             //    too long string is cut off
             //    fs is padded with zeros only if there is space
{ let i = 0, c;
  while i < chars do
  { c := byte i of s;
    if c = 0 then break;
    byte i of fs := c;
    i +:= 1 }
    while i < chars do
  { byte i of fs := 0;
    i +:= 1 } }

let string_ftov(fs, s, chars) be
             // opposite of _vtof
{ let i = 0, c;
  while i < chars do
  { c := byte i of fs;
    if c = 0 then break;
    byte i of s := c;
    i +:= 1 }
  byte i of s := 0 }

let zerob(b) be    // fill a whole blocks-worth of memory with zeros
  for i = 0 to 127 do
    b ! i := 0;

let atol(s) be     // e.g. atol("5362") = 5362, no error checking
{ let i = 0, n = 0;
  while true do
  { let c = byte i of s;
    if c = 0 then break;
    n := n * 10 + c - '0';
    i +:= 1 }
  resultis n }

let out_time(t) be    // takes date-time in seconds and prints it nicely
{ let v = vec 7;
  datetime(t, v);
  out("%d-%02d-%02d %02d:%02d:%02d", v!0, v!1, v!2, v!4, v!5, v!6) }

manifest
{                 // superblock contents
  sb_numblocks = 0,     // size of disc
  sb_rootdir = 1,       // which block is the root directory in
  sb_firstfree = 2,     // which is the first free block
  sb_disc_name = 3,          // name of disc

                  // directory entries
  de_name = 0,          // name comes first, up to 12 chars, and "fs"
  de_block = 3,         // de ! 3 = the file's header block number
  de_namelen = 12,      // max chars in name
  de_namewords = 4,     // words reuqired for a regular (zero-terminated) string
  de_size = 4,
  de_per_block = 128 / de_size,

                  // file header blocks
  fhdr_cre_time = 0,    // file's creation date-time
  fhdr_mod_time = 1,    // date-time of last modification
  fhdr_blocks = 2,      // number of blocks the file occupies
  fhdr_length = 3,      // file's content length in bytes

                  // FILE* structures
  nfiles = 4,           // 4 files can be open at once
  filestarsize = nfiles+1;
  fstr_index = 0,       // is this file 1, 2, 3, or 4?
  fstr_used = 1,        // is this FILE* is use (i.e. a file is open)
  fstr_buffer = 2,      // address of the 512 byte buffer
  fstr_hdr_block = 3,   // which disc block contains the file's header
  fstr_curr_block = 4,  // which block is currently in the buffer
  fstr_max_block = 5,   // last block the file may occupy
  fstr_buff_pos = 6,    // current position within the buffer
  fstr_buff_max = 7,    // maximum value for buff_pos
  fstr_length = 8,      // length in bytes safely on disc
  fstr_can_write = 9,   // true means writable, false means readable
  fstr_size = 10 }

static
{ superblock = nil,     // in-memory copy of the superblock
  rootdir = nil,        // in memory copy of the root directory
                        // pointers to all the FILE* objects
  filestars = vec filestarsize }

let write_block(bn, mem) be
{ let r = devctl(dc_disc_write, 1, bn, 1, mem);
  if r < 1 then
    out("Error %d on trying to write block %d\n", r, bn) }

let read_block(bn, mem) be
{ let r = devctl(dc_disc_read, 1, bn, 1, mem);
  if r < 1 then
    out("Error %d on trying to read block %d\n", r, bn) }

//first function call should be this
let format(disc_num,disc_name) be
{ let r = devctl(dc_disc_check, 1);
  out("%d blocks avalable\n", r);
  if superblock = nil then // if the superblock is null
    superblock := newvec(128);// create the superblock
  zerob(superblock); // zero out the superblock
  superblock ! sb_numblocks := r; // set the number of blocks in the superblock
  superblock ! sb_rootdir := 1; // set the root dir
  superblock ! sb_firstfree := 2;// set the first freeblock
  superblock ! sb_disc_name := disc_name;// set disc name
  write_block(0, superblock);
  if rootdir = nil then
    rootdir := newvec(128);
  zerob(rootdir);
  write_block(1, rootdir) }

//second function call should be this
let mount() be
{ if superblock = nil then 
    superblock := newvec(128);
  read_block(0, superblock);
  out("%d blocks\n", superblock ! sb_numblocks);
  out("first free block %d\n", superblock ! sb_firstfree);
  out("root dir in block %d\n", superblock ! sb_rootdir);
  if rootdir = nil then
    rootdir := newvec(128);
  read_block(superblock ! sb_rootdir, rootdir) }

let shut_down() be
{ write_block(0, superblock);
  write_block(1, rootdir);
  finish }

let create(fn, bsz) be
{ let pde, i, hdr = vec 128;
  i := 0;
  while i < 128 do
  { pde := rootdir + i;
    if pde ! 0 = 0 then break;
    i +:= de_size }
  if i >= 128 then
  { outs("root directory is full\n");
    resultis -1 }
  string_vtof(fn, pde + de_name, de_namelen); 
                               // does not check for full disc
  pde ! de_block := superblock ! sb_firstfree;
  superblock ! sb_firstfree +:= bsz;
  zerob(hdr);
  hdr ! fhdr_cre_time := seconds();
  hdr ! fhdr_mod_time := hdr ! fhdr_cre_time;
  hdr ! fhdr_blocks := bsz;
  hdr ! fhdr_length := 0;
  write_block(pde ! de_block, hdr) }

let find(fn) be
                  // returns block number for header block
                  // or 0 for failure
{ let pde, i, ename = vec(de_namewords);
  i := 0;
  while i < 128 do
  { pde := rootdir + i;
    if pde ! 0 <> 0 then
    { string_ftov(pde + de_name, ename, de_namelen);
      if str_eq(ename, fn) then
        resultis pde ! de_block }
    i +:= de_size }
  resultis 0 }

let file(p1) be
{ let b = find(p1), hdr = vec(128);
  if b = 0 then
  { out("file '%s' not found\n", p1);
    return }
  read_block(b, hdr);
  out("file '%s':\n", p1);
  outs("   created: "); out_time(hdr ! fhdr_cre_time); outch('\n');
  outs("  modified: "); out_time(hdr ! fhdr_mod_time); outch('\n');
  out("   hdr blk: %d\n", b);
  out("  occupies: %d blocks\n", hdr ! fhdr_blocks);
  out("  contents: %d bytes\n", hdr ! fhdr_length) }

let dir() be
{ let pde, i, ename = vec(de_namewords);
  i := 0;
  while i < 128 do
  { pde := rootdir + i;
    if pde ! 0 <> 0 then
    { string_ftov(pde + de_name, ename, de_namelen);
      out("file '%s', block %d\n", ename, pde ! de_block) }
    i +:= de_size } }

let get_filestar(n) be     
              // get_filestar(): find a free FILE* struct to use
              // get_filestar(n): return nth FILE* if it exists
{ if numbargs() > 0 then
  { test n > 0 /\ n <= nfiles then
      resultis filestars ! n
    else
      resultis nil }
              // filestar!i is nil: unused slot - create a FILE * 
              //      and use it
              // not nil: check fstr_used field
  for i = 1 to nfiles do
  { let fs = filestars ! i;
    if fs = nil then
    { fs := newvec(fstr_size);
      fs ! fstr_buffer := newvec(128);
      fs ! fstr_used := true;
      fs ! fstr_index := i;
      filestars ! i := fs;
      resultis fs }
    if not (fs ! fstr_used) then
    { fs ! fstr_used := true;
      resultis fs } }
  outs("All FILE*s are in use\n");
  resultis nil }

let file_empty_buffer(fs) be
                            // write file's buffer to disc, and return it
                            // as an empty buffer for reuse
{ if fs ! fstr_curr_block > fs ! fstr_max_block then
  { outs("file maximum size exceeded\n");
    resultis 0 }
  fs ! fstr_length +:= fs ! fstr_buff_pos;
                            // when writing, fstr_length is the number of bytes
                            // actually on disc, does not include those in buffer
  write_block(fs ! fstr_curr_block, fs ! fstr_buffer);
  fs ! fstr_curr_block +:= 1;
  fs ! fstr_buff_pos := 0;
  fs ! fstr_buff_max := 512;
  resultis 1 }

let file_refill_buffer(fs) be
                            // when reading buffer is empty, must refill it
                            // with the next block from disc
{ if fs ! fstr_length <= 0 then
    resultis 0;
  fs ! fstr_curr_block +:= 1;
  fs ! fstr_buff_pos := 0;
  fs ! fstr_buff_max := 0;
  if fs ! fstr_curr_block > fs ! fstr_max_block then
    resultis 0;
  read_block(fs ! fstr_curr_block, fs ! fstr_buffer);
                            // when reading, fstr_length is number of bytes
                            // left on disc, excluding those in the buffer
  test fs ! fstr_length >= 512 then
    fs ! fstr_buff_max := 512
  else if fs ! fstr_length > 0 then
    fs ! fstr_buff_max := fs ! fstr_length;
  fs ! fstr_length -:= fs ! fstr_buff_max;
  resultis 1 }

let open(fnm, rwa) be
{ let b = find(fnm), hdr = vec(128), fs;
  if b < 0 then
  { out("file '%s' not found\n", fnm);
    resultis 0 }
  rwa := byte 0 of rwa;
  if rwa<>'r' /\ rwa<>'w' /\ rwa<>'a' then
  { outs("mode of r, w, or a required\n");
    resultis 0 }
  read_block(b, hdr);
  fs := get_filestar();
  if fs = nil then resultis 0;
  fs ! fstr_hdr_block := b;
  fs ! fstr_max_block := b + hdr ! fhdr_blocks - 1;
        /* reading
             read first content block into buffer
             buffer position is beginning of buffer
             length is total length of contents
           writing - original file contents will be lost
             start with empty buffer, 
             position at beginning
             length is zero
           appending - original contents left
             read last content block into buffer
             buffer position after end of contents
             length excludes those characters in the buffer
        */
  if rwa = 'r' then
  { fs ! fstr_curr_block := b + 1;
    fs ! fstr_buff_pos := 0;
    read_block(fs ! fstr_curr_block, fs ! fstr_buffer);
    test hdr ! fhdr_length < 512 then
      fs ! fstr_buff_max := hdr ! fhdr_length
    else
      fs ! fstr_buff_max := 512;
    fs ! fstr_length := hdr ! fhdr_length - fs ! fstr_buff_max;
    fs ! fstr_can_write := false;
    resultis fs ! fstr_index }

  if rwa = 'w' then
  { fs ! fstr_curr_block := b + 1;
    fs ! fstr_buff_pos := 0;
    fs ! fstr_length := 0;
    fs ! fstr_buff_max := 512;
    fs ! fstr_can_write := true;
    resultis fs ! fstr_index }

  if rwa = 'a' then
  { fs ! fstr_curr_block := b + 1 + (hdr ! fhdr_length) / 512;
    fs ! fstr_buff_pos := (hdr ! fhdr_length) rem 512;
    fs ! fstr_length := (hdr ! fhdr_length) bitand (bitnot 511);
    read_block(fs ! fstr_curr_block, fs ! fstr_buffer);
    fs ! fstr_buff_max := 512;
    fs ! fstr_can_write := true;
    resultis fs ! fstr_index } }

let close(fn) be 
{ let fs = get_filestar(fn), hdr = vec(128);
  if fs = nil then
  { outs("no such FILE*\n");
    resultis 0 }
  unless fs ! fstr_can_write do
  { fs ! fstr_used := false;
    resultis 1; }
  if fs ! fstr_buff_pos > 0 then
    file_empty_buffer(fs);
  read_block(fs ! fstr_hdr_block, hdr);
  hdr ! fhdr_mod_time := seconds();
  hdr ! fhdr_length := fs ! fstr_length;
  write_block(fs ! fstr_hdr_block, hdr);
  fs ! fstr_used := false;
  resultis 1; }

let state(fn) be
{ let fs = get_filestar(fn);
  if fs = nil then
  { outs("no such FILE*\n");
    return }
  out("      fstr_index = %d\n", fs ! fstr_index);
  out("       fstr_used = %d\n", fs ! fstr_used);
  out("     fstr_buffer = %d\n", fs ! fstr_buffer);
  out("  fstr_hdr_block = %d\n", fs ! fstr_hdr_block);
  out(" fstr_curr_block = %d\n", fs ! fstr_curr_block);
  out("  fstr_max_block = %d\n", fs ! fstr_max_block);
  out("   fstr_buff_pos = %d\n", fs ! fstr_buff_pos);
  out("   fstr_buff_max = %d\n", fs ! fstr_buff_max);
  out("     fstr_length = %d\n", fs ! fstr_length);
  out("  fstr_can_write = %d\n", fs ! fstr_can_write) }

let fwrite_char(fn, c) be
{ let fs = get_filestar(fn);
  if fs = nil then
  { outs("no such FILE*\n");
    resultis 0 }
  unless fs ! fstr_can_write do
  { out("file* %d not writeable\n", fn);
    resultis -1 }
  byte fs ! fstr_buff_pos of fs ! fstr_buffer := c;
  fs ! fstr_buff_pos +:= 1;
  if fs ! fstr_buff_pos >= 512 then
    file_empty_buffer(fs);
  resultis c }

let fwrite_num(fn, n) be
{ if n > 9 then
    fwrite_num(fn, n/10);
  fwrite_char(fn, '0' + n rem 10) }

let fwrite_num_digs(fn, n, d) be
{ if d > 1 then
    fwrite_num_digs(fn, n/10, d-1);
  fwrite_char(fn, '0' + n rem 10) }

let fwrite_str(fn, s) be
{ let i = 0, c;
  while true do
  { c := byte i of s;
    if c = 0 then break;
    fwrite_char(fn, c);
    i +:= 1 } }

let fread_char(fn) be
{ let fs = get_filestar(fn), c;
  if fs = nil then
  { outs("no such FILE*\n");
    resultis -1 }
  if fs ! fstr_can_write then
  { out("file* %d not readable\n", fn);
    resultis -1 }
  if fs ! fstr_buff_pos >= fs ! fstr_buff_max then
    file_refill_buffer(fs);
  if fs ! fstr_buff_pos >= fs ! fstr_buff_max then
    resultis -1;
  c := byte fs ! fstr_buff_pos of fs ! fstr_buffer;
  fs ! fstr_buff_pos +:= 1;
  resultis c }

let write_to_file(fn) be
{ let c;
  while true do
  { c := inch();
    if c = '*' then break;
    fwrite_char(fn, c) }
  while c <> '\n' do
    c := inch() }

let read_from_file(fn, n) be
{ let c;
  if n = 0 then n :=0x7FFFFFFF;
  while n > 0 do
  { c := fread_char(fn);
    if c = -1 then break;
    outch(c);
    n -:= 1 }
  outch('\n') }

let table_to_file(fn, n) be
{ for c = 1 to n do
  { fwrite_num_digs(fn, c, 3);
    fwrite_str(fn, " degrees centigrade is ");
    fwrite_num_digs(fn, 32 + c*9/5, 3);
    fwrite_str(fn, " degrees fahrenheit.\n"); } }

let command(parts, np) be
{ let p0 = parts ! 0, p1 = "", p2 = "";
  if np > 1 then p1 := parts ! 1;
  if np > 2 then p2 := parts ! 2;
  test str_eq(p0, "exit") then
    shut_down()        // save changes to superblock and rootdir and exit
  else test str_eq(p0, "format") then
  {
    out("%s\n",parts!1);
    format(parts!1)           // start again - reformat whole disc
  }
  else test str_eq(p0, "mount") then
    mount()            // prepare to use the disc - required
  else test str_eq(p0, "create") then
    create(p1, atol(p2))    // create <name> <nb> - create new file
                       // nb = number of blocks to occupy, inc. header
  else test str_eq(p0, "dir") then
    dir()              // list file names and header block numbers
  else test str_eq(p0, "file") then
    file(p1)           // file <name> - print header block info
  else test str_eq(p0, "open") then
                       // open <name> [r|w|a]
                       //    returns FILE* index <fn>
  { let fs = open(p1, p2);
    if fs > 0 then
      out("'%s' open as FILE* %d\n", p1, fs) }
  else test str_eq(p0, "close") then
    close(atol(p1))    // close <fn>
  else test str_eq(p0, "state") then
    state(atol(p1))    // state <fn> - list info on open file
  else test str_eq(p0, "write") then
    write_to_file(atol(p1))   // write <fn> - add characters to file- user
                              // types them, terminated by *
  else test str_eq(p0, "read") then
                              // read <fn> - show entire file contents
                              // read <fn> <n> - show next n chars
    read_from_file(atol(p1), atol(p2))
  else test str_eq(p0, "table") then   
                              // table <fn> <lines> - add <lines> lines of
                              //    a C to D conversion table to file <fn>
    table_to_file(atol(p1), atol(p2))
  else test str_eq(p0, "echo") then
    for i = 0 to np-1 do
      out("parts!%d = \"%s\"\n", i, parts!i)
  else
    out("unrecognised command '%s'\n", p0) 
}

let command_loop() be
{ manifest
  { max_bytes = 120,
    max_chars = 112,
    max_parts = 6 }
  let line = vec max_bytes/4, part = vec max_parts, nparts, c, i;
  while true do
  { nparts := 0;
    i := 0;
    outs("> ");
    c := inch();
    while true do
    { while c = ' ' do
        c := inch();
      if c = '\n' then 
        break;
      if nparts < max_parts then
      { part ! nparts := line + i/4;
        nparts +:= 1; }
      while c > ' ' do
      { if i < max_chars then
        { byte i of line := c;
          i +:= 1; }
        c := inch() }
      { if i < max_bytes then
          byte i of line := 0;
        i +:= 1 }
          repeatuntil i rem 4 = 0; }
    command(part, nparts) } }

let start() be
{ make_heap();
  for i = 1 to nfiles do
    filestars ! i := nil;
  command_loop(); } 
