package iew.prep.recursion;

public class AVLTreeNode {
	public int val;   //Value
    public int ht;      //Height
    public AVLTreeNode left;   //Left child
    public AVLTreeNode right;   //Right child
    
    @Override
    public String toString() {
    	return "{val:"+val+",ht:"+ht+"}";
    }
    
    
}
