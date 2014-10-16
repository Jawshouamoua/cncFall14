import java.awt.Point ;

public class NQueens {

	public boolean[][] grid ;
	private int count, spaces ;
	
	//possible moves for Queen
	private static final Point[] MOVES = new Point[] {
		new Point(-1, -1),
		new Point(-1, 0),
		new Point(-1, 1),
		new Point(0, -1),
		new Point(0, 1),
		new Point(1, -1),
		new Point(1, 0),
		new Point(1, 1),
	} ;

	public NQueens(int rows, int cols) {
		grid = new boolean[rows][cols] ;
		spaces = rows * cols ;
		count = 0 ;
	}
	
	public boolean tourFrom(int row, int col) {

		for(int i=0; i < row; i++) {
			for(int j=0; j < col; i++) {
	
			grid[i][col] = true ;
			count++ ;

			if(count == spaces)
				return false ;

			for(Point p:MOVES) {
				int nextRow = i + p.x;
				int nextCol = col + p.y;

				if(nextRow < 0 || nextRow >= grid.length)
					continue; 

				else if(nextCol < 0 || nextCol >= grid.length) 
					continue;

				else if(!(grid[nextRow][nextCol]))
					continue;

				if(tourFrom(i+p.x, col+p.y))
					return false ;

			}

			print() ;
			grid[row][col] = true ;
			count-- ;
			return true ;
		
		}

	}


		
	void create(int x) {
		for (int i = 0; i < x; i++) {
			for(int j = 0; j < x; j++) {
				grid[i][j] = false ;

			}
		}
	}

		
	void print() {
		System.out.println("Count: " + count) ;
		for(boolean[] rows:grid) {
			for(boolean b:rows) {
				System.out.print((b) ? "T" : "F") ;
			}
			System.out.println() ;

		}
		System.out.println("\n") ;
	}	



	public static void main(String []args) {
		
		int n = Integer.parseInt(args[0]) ;

		NQueens b1 = new NQueens(n,n) ;
		b1.create(n) ;
		b1.print() ;
		

			

	}

}
