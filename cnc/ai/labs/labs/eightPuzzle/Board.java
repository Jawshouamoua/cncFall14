import java.util.* ;
import java.lang.* ;

public class Board {
	

	private int[][] board ;
	private int[][] target = {{0,1,2},{3,4,5},{6,7,8}} ;
	private int moves = 0 ;
	private int zRow = 0 ;
	private int zCol = 0 ;
	private int boardSize ;
	static Comparator<Board> comp = new BoardComparator() ;

	//construct board [i][j] where i row j is col
	public Board(int[][] blocks, int nMoves) {
		
		moves = nMoves ;
		board = blocks ;	
		boardSize = board.length ;
		
		//find zero position
		for(int i=0;i<boardSize;i++) {
			for(int j=0;j<boardSize;j++) {
				if (board[i][j] == 0) {
					zRow = i ;
					zCol = j ;
				}
			}
		}

	}

	public int moves() {
		return moves ;
	}

	public int size() {

		int size = board.length ;
	
		return size ;
	}


	public int hamming() {

		int hamming = 0 ;
		int n = this.size() ;
		
		for(int i=0; i<n;i++) {
			for(int j=0; j<n;j++) {
				if(board[i][j] != target[i][j]) 
					hamming++ ;
			}
		}

		return hamming ;

	}

	public int manhattan() {

		int row = 0 ;
		int col = 0 ;
		int totCnt = 0 ;
		int n = this.size() ;

		for(int i=0; i<n; i++) {
			for(int j=0; j<n; j++) {
				
				for(int k=0; k<n; k++) {
					for(int l=0; l<n; l++) {
						
						if(target[i][j] == board[k][l]) {
							row = k ;
							col = l ;
						}
					}
				}

				totCnt += (Math.abs(row-i)) ;
				totCnt += (Math.abs(col-j)) ;

			}
		}

		return totCnt ;
	
	}

	public int zPosRow() {
		return zRow ;
	}
	public int zPosCol() {
		return zCol ;
	}

	public boolean isGoal() {

		if(Objects.deepEquals(board,target))
			return true ;
		else
			return false ;	

	}

	public boolean isSolvable() {

		int[] l = new int[(int)Math.pow(boardSize,2)] ;
		int inv = 0 ;
		int k = 0 ;

		for(int i=0;i<boardSize;i++) {
			for(int j=0;j<boardSize;j++) {
				if(board[i][j]==0)
					continue ;
				l[k] = board[i][j] ;
				k++ ;
			}
		}
	

		//count inversions
		for(int i=0;i<l.length;i++) {
			for(int j=i;j<l.length;j++) {
				if(l[i] > l[j])
					inv++ ;	
			}
		}
		
		System.out.println("inversions: " + inv) ;
		
		if(inv%2==0)
			return true ;
		else
			return false ;
	
	}

	public int[][] getBoard() {
		return board ;
	}

	public boolean equals(Board y) {

		if(Objects.deepEquals(this.getBoard(),y.getBoard()))
			return true ;
		else
			return false ;
	}

	public int[][] bInit(int[][] b) {

		int[][] b1 = new int[boardSize][boardSize] ;
		for(int i=0;i<boardSize;i++) {
			for(int j=0;j<boardSize;j++) {
				b1[i][j] = b[i][j] ;
			}

		}
		return b1 ;

	}

	public Iterable<Board> neighbors() {

		PriorityQueue<Board> nb = new PriorityQueue<Board>(comp) ;
		int[][] b1 = new int[boardSize][boardSize] ;
		Board neighbor ; 
		
		b1 = this.bInit(board) ;
		//move left
		if(zCol > 0) {
			b1[zRow][zCol] = b1[zRow][zCol-1] ;
			b1[zRow][zCol-1] = 0 ;
			neighbor = new Board(b1,this.moves()+1) ;
			//System.out.println(neighbor.print()+"left") ;
			nb.add(neighbor) ;
		}
		b1 = this.bInit(board) ;
		
		//move right
		if(zCol < boardSize-1) {
			b1[zRow][zCol] = b1[zRow][zCol+1] ;
			b1[zRow][zCol+1] = 0 ;
			neighbor = new Board(b1,this.moves()+1) ;
			//System.out.println(neighbor.print()+"right") ;
			nb.add(neighbor) ;
		}
		b1 = this.bInit(board) ;

		//move up
		if(zRow > 0) {
			b1[zRow][zCol] = b1[zRow-1][zCol] ;
			b1[zRow-1][zCol] = 0 ;
			neighbor = new Board(b1,this.moves()+1) ;
			//System.out.println(neighbor.print()+"up") ;
			nb.add(neighbor) ;
		}	
		b1 = this.bInit(board) ;

		//move down
		if(zRow < boardSize-1) {
			b1[zRow][zCol] = b1[zRow+1][zCol] ;
			b1[zRow+1][zCol] = 0 ;
			neighbor = new Board(b1,this.moves()+1) ;
			//System.out.println(neighbor.print()+"down") ;
			nb.add(neighbor) ;
		}

		return nb ;

	}

	public String print() {

		String s = "" ;
		for(int i=0; i<boardSize; i++) {
			for(int j=0; j<boardSize; j++) {
				s = s + Integer.toString(board[i][j]) + " " ;
			}
			s = s + "\n" ;
		}
		s = s + "moves: " + moves ;
		return s ;

	}

	public static void main(String[] args) {

		//int[][] newBoard = {{0,1,2},{3,4,5},{6,7,8}} ;
		int[][] newBoard = {{1,2,3},{4,0,6},{8,5,7}} ;
		PriorityQueue<Board> pQueue = new PriorityQueue<Board>(comp) ; 
		Iterable<Board> holder ; 

		Board b1 = new Board(newBoard,0) ;
		System.out.println(b1.print()) ;
		System.out.println("(" + b1.zPosRow() + "," + b1.zPosCol() + ")" ) ;
		
		if(b1.isSolvable()) 
			System.out.println("Solvable") ;

		holder = b1.neighbors() ;
		for(Board b : holder) 
			pQueue.add(b) ;

		b1 = pQueue.poll() ;
		System.out.println(b1.print()) ;
		System.out.println("(" + b1.zPosRow() + "," + b1.zPosCol() + ")" ) ;

		b1 = pQueue.poll() ;
		System.out.println(b1.print()) ;
		System.out.println("(" + b1.zPosRow() + "," + b1.zPosCol() + ")" ) ;

	}

}
