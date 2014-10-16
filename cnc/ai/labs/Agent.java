public class Agent extends Environment {

	public String state = "" ;

	public getState {
		state = Environment.currentState ;
	}

	public void act {
		if(state == "dirty") 
			this.clean() ;

		else {
			if(Agent.canMove("left"))
				this.move("left") ;

			else
				this.move("right") ;
		}

	public void canMove(String direction) {
		
		
