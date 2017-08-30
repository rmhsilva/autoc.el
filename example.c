
// autoc examples


/*
  autoc:defun func1 (x)
  (dotimes (n x)
    (insert "void;")
    (newline-and-indent))
  autoc#
*/

// Result:
// autoc:funcall func1 3
void;
void;
void;
// autoc#




//autoc:block block1
// some content
// across multiple lines
//autoc#

//autoc:lines lines1
// some content
// across multiple lines
//autoc#


//autoc:format-lines lines1 "%s WOO"
// some content WOO
// across multiple lines WOO
//autoc#

//autoc:message "hello world woooo"

//autoc#
