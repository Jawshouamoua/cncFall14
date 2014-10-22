import java.awt.Point ;

public class Board {

	public boolean[][] grid ;
	private int count, spaces ;

	//possible moves for queen
	private static final Point[] MOVES = new Point[] {
		new Point(-1, -1),
		new Point(-1, 0),
		new Point(-1, 1),
		new Point(0,-1),
		new Point(0, 1),
		new Point(1, -1),
		new Point(1, 0),
		new Point(1, 1),
	} ;

	public Board(int rows, int cols) {
		grid = new boolean[rows][cols] ;
		spaces = rows * cols ;
		count = 0 ;
		
	}

	public boolean check(int row, int col, int n) {

		for(int i=0; i<n; i++) {
			
			int nextRow = (i + row)%n ;
			int nextCol = (i + col)%n ;

			if(i==row || i==col)
				continue ; 

			else if( grid[nextRow][col] ||
					 grid[row][nextCol] ||
					 grid[nextRow][nextCol] )
					
				return true ;	

		}

		return false ;
			
	}

	

	public boolean tourFrom(int row, int col) {

		int n = grid.length ;
		int j = col ;
		
//		grid[row][col] = true ;
//		count++ ;

		//current position of queen
		for(int i=0; i < n; i++) {

			System.out.println(i) ;


			grid[row][col] = true ;
			count++ ;
			//for(int j=0; j<n; j++) {
				

				if(count == spaces) 
					return false ;

				else if(check(i,j,n))
					continue ;

				else if(j>grid.length)
					continue ;

				else if(i>grid.length)
					continue ;

				grid[row][col] = true ;
				count++ ;

				if(tourFrom(i,j+1)) 
					return true ;

			//}
		
		
		grid[row][col] = false ;
		print() ;
		count-- ;

		}

		return false ;

/*		print() ;
		grid[row][col] = false ;
		count-- ;
		return false ;
*/
	}


	void print() {
		System.out.println("Count: " + count) ;
		for(boolean[] rows:grid) {
			for(boolean b:rows) 
				System.out.print((b) ? "T":"F") ;

			System.out.println() ;

		}
		System.out.println("\n") ;
	}


	public static void main(String []args) {
		
		int n = Integer.parseInt(args[0]) ;
		Board b1 = new Board(n,n) ;
		b1.tourFrom(0,0) ;

		
	}



}




	
				
