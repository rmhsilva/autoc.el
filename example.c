// autoc examples


/*
  autoc:defun func1 (x)
  (dotimes (n x)
    (insert "void;")
    (newline-and-indent))
  autoc_end
*/

// Result:
// autoc:funcall func1 3
void;
void;
void;
// autoc_end




//autoc:block block1
some;
text;
//autoc:end
