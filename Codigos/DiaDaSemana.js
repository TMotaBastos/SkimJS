function diaSemana(a){
  switch(a){
    case 1:
      return "Domingo";
    case 2:
      return "Segunda-Feira";
    case 3:
      return "Terça-Feira";
    case 4:
      return "Quarta-feira";
    case 5:
      return "Quinta-feira";
    case 6:
      return "Sexta-feira";
    case 7:
      return "Sábado";
  }
}

var a = 3;
var x = diaSemana(a);
x;
