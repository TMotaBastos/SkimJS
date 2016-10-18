function palindrome(a){
  var b = [];
  var i = 0;
  for (i; i<a.length; i++){
    b = b.concat(a[a.length - i - 1]);
  }

  return b.equals(a);
}

var a = [5,7,5];
var x = palindrome(a);
x;
