class Drawable {
      public void draw() {
            System.out.println("Generic drawable object") ;
      }
       
      public static void main(String [] argv) {
            System.out.println("Shuffling");
            Drawable x ;
                  if ( argv[0].equals("c") ) {
                        x = new Circle(10,10,10) ;
                  } else {
                    x = new Square(5,5,5) ;
                  }
            x.draw() ; 
      }
}
class Square extends Drawable {
       int x, y, side ;

       Square(int x, int y, int side) {
             this.x = x ;
             this.y = y ;
             this.side = side ;
       }
       public void draw() {
             System.out.println("Square with corner at ("
                                       +x+","+y+") and side "+side) ;
      } 
}
class Circle extends Drawable {
      int x, y, radius ;
      Circle(int x, int y, int radius) {
             this.x = x ;
             this.y = y ;
             this.radius = radius ;
      }
      public void draw() {
            System.out.println("Circle with center at ("+x+","+y+") and radius "+radius) ;
      }      
}
















