

class Account {
    int[] accounts;

    //O(n)
    boolean log (int pass) {
	int l = accounts.length;
	for(int i = 1; i < l; i++){
	    if(pass == accounts[i]){
		return true;
	    }
	    else {}
	}
	return false;
    }
}




class Login {

    Account a = new Account();

    public void main(String[] args) {
        if(args.length == 0){
            System.out.println("O(n * m)");
        }else{
            a.log(args[0].length());
        }
    }
}
