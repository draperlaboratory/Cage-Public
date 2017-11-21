public class HashTable{
    private Bucket[] buckets;

    public HashTable(int bucketCount){
        this.buckets = new Bucket[bucketCount];
    }

    public boolean contains(String s){
        int i = this.index(s);
        if(this.buckets[i] == null){
            return false;
        }else{
            return this.buckets[i].contains(s);
        }
    }

    public void add(String s){
        int i = this.index(s);
        if(this.buckets[i] == null){
            this.buckets[i] = new Bucket(s);
        }else{
            this.buckets[i].add(s);
        }
    }

    public void remove(String s) throws NotFound {
        int i = this.index(s);
        if(this.buckets[i] == null){
            // element not in the table, uh oh.
            throw new NotFound(s);
        }else{
            this.buckets[i].remove(s);
        }
    }

    private int index(String s){
        return Hash.hash(s) % this.buckets.length;
    }
}
