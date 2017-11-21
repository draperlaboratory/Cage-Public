

class HashTbl {

    private List[] table;

    //constant
    public HashTbl () {

	table = new List[] { new Nil(), new Nil(), new Nil() };
    }

    //O(n), where n is the length of the list @ table[index]
    public void insert(int x) {
	int hash_x = hashVal(x);
	table[hash_x] = table[hash_x].append(x);
    }

    //constant
    private int hashVal(int y) {
	return y % 3;
    }
}
