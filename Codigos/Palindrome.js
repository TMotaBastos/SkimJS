function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}

function palindrome(a){
  var b = [];
  var i = 0;
  for (i; i<len(a,0); i++){
    b = b.concat(a[len(a,0) - i - 1]);
  }

  return b.equals(a);
}

var a = [5,7,5];
var x = palindrome(a);
x;
