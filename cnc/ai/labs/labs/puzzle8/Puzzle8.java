
import java.util.* ;
import java.lang.* ;

public class Puzzle8 {

	int[][] board ;
	int[][] target = {  {1,2,3},
						{4,5,6},
						{7,8,0}	} ;

	int zRow, zCol ;

	Puzzle8() {
		
		board = new int[3][3] ;
		List<Integer> seed = new ArrayList<Integer>() ;
		for(int i = 0; i < 9; i++) 
			seed.add(i) ;
			
		Collections.shuffle(seed) ;
		int count = 0 ;
		for(int i = 0; i < 3; i++) {
			for(int j = 0; j < 3; j++) {
				if(seed.get(count)==0) {
					zRow = i;
					zCol = j;
				}

				board[i][j] = seed.get(count) ; count++ ; 
			}

		}

	}
		
	int hamming() {

		int score = 0 ;

		for(int i=0;i<3;i++) {
			for(int j=0;j<3;j++) {
				if(board[i][j] != target[i][j])
					score++ ;	
			
			}
		}

		return score ;

	}


	int manhattan() {

		int row = 0 ;
		int col = 0 ;
		int totCnt = 0 ;

		for(int i=0;i<3;i++) {
			for(int j=0;j<3;j++) {
				
				for(int k=0;k<3;k++) {
					for(int l=0;l<3;l++) {
						
						if(target[i][j] == board[k][l]) {
							row = k; 
							col = l;
						}
					}
				}

				totCnt += (Math.abs(row - i)) ;
				totCnt += (Math.abs(col - j)) ;

			}
		}

		return totCnt ;
										
	}

			
	void printBoard() {
		
		for(int i=0; i<3; i++) {
			for(int j=0; j<3; j++) {
				System.out.print(board[i][j] + "\t") ;
			}
			System.out.println() ;
		}
		System.out.println("\n") ;
		System.out.println("hamming = " + this.hamming()) ;
		System.out.println("manhattan = " + this.manhattan()) ;
		System.out.println("Total = " + (this.hamming() + this.manhattan())) ;
		
	

	}

	

	void move(String direction) {

		switch (direction) {
		
		case "left":	if(zCol > 0) {
							board[zRow][zCol] = board[zRow][zCol-1] ;
							board[zRow][zCol-1] = 0 ;
							zCol -= 1 ;
						}
						break ;
		
		case "right":	if(zCol < 2) {
							board[zRow][zCol] = board[zRow][zCol+1] ;
							board[zRow][zCol+1] = 0 ;
							zCol += 1 ;
						}
						break ;
		
		case "up":		if(zRow > 0) {
							board[zRow][zCol] = board[zRow-1][zCol] ;
							board[zRow-1][zCol] = 0 ;
							zRow -= 1 ;
						}
						break ;

		case "down":	if(zRow < 2) {
							board[zRow][zCol] = board[zRow+1][zCol] ;
							board[zRow+1][zCol] = 0 ;
							zRow += 1 ;
						}
						break ;

		}
		
	

	}
	
	boolean canMove(String direction) {
		
		switch (direction) {
			case "left":
				if(zCol>0) 
					return true ;

			case "right":
				if(zCol<2)
					return true ;
			
			case "up":
				if(zRow>0)
					return true ;

			case "down":
				if(zRow<2) 
					return true ;

			default:
				return false ;
		}

	}

	void makeMove() {

		int[][] tempBoard = board ;
/*
		int lHam, rHam, uHam, dHam ;
		int lMan, rMan, uMan, dMan ;
		int tLeft,tRight,tUp,tDown ;
*/

		int lowestScore = 46 ;
		String bestMove = "" ;

		String[] direction = {"left","right","up","down"} ;
		int[] hamScore = new int[4] ;
		int[] manScore = new int[4] ;
		int[] totScore = new int[4] ;
		
		for(int i=0;i<4;i++) {
			this.move(direction[i]) ;
			//this.printBoard() ;
			if(this.canMove(direction[i])) {
				hamScore[i] = this.hamming() ;
				manScore[i] = this.manhattan() ;
				totScore[i] = hamScore[i] + manScore[i] ;
			}
			else {
				hamScore[i] = 9 ;
				manScore[i] = 36 ;
				totScore[i] = hamScore[i] + manScore[i] ;
			}
			board = tempBoard ;
		}
		
		for(int i=0;i<4;i++) {
		//	System.out.println("lowest score" + lowestScore) ;
			if(totScore[i] < lowestScore) {
				lowestScore = totScore[i] ;
				bestMove = direction[i] ;
			}
		}
			
		//System.out.println("best move = " + bestMove) ;
		this.move(bestMove) ;
		this.printBoard() ;

	}

/*
		this.move("left") ;
		if(board!=tempBoard) {
			lHam = this.hamming() ;
			lMan = this.manhattan() ;
		}
		board = tempBoard ;
		
		this.move("right") ;
		if(board!=tempBoard) {
			rHam = this.hamming() ;
			rMan = this.manhattan() ;
		}
		board = tempBoard ;

		this.move("up") ;
		if(board!=tempBoard) {
			uHam = this.hamming() ;
			uMan = this.manhattan() ;
		}
		board = tempBoard ;

		this.move("down") ;
		if(board!=tempBoard) {
			dHam = this.hamming() ;
			uMan = this.manhattan() ;
		}
		board = tempBoard ;

		tLeft = lHam + lMan ;
		tRight = rHam + rMan ;
		tUp = uHam + uMan ;
		tDown = dHam + dMan ;
*/

	boolean check() {
		if(board!=target)
			return true ;
		else
			return false ;
	}
		

	public static void main(String[] args) {
	
		Puzzle8 game = new Puzzle8() ;
		game.printBoard() ;
		for(int i=0;i<3;i++) {
			game.makeMove() ;	
		}
		System.out.println("done") ;
	}
		


}
		
