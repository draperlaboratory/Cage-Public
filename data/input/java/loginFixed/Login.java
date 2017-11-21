
class Login {

    Account a = new Account();

    public void main(String[] args) {
        if(args.length == 0){
            System.out.println("O(n)");
        }else{
            a.login(args[0].length());
        }
    }
}
