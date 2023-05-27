// Pos.java
// AoC 2018
// Author: Chi-Kit Pao
//


package net.chikitpao.util;

public class Pos {
    public int y;
    public int x;
    public Pos(int y, int x) {
        this.y = y;
        this.x = x;
    }
    public Pos(Pos other){
        this.y = other.y;
        this.x = other.x;
    }

    @Override
    public boolean equals(Object obj) {
        if(obj == null)
            return false;
        if(!(obj instanceof Pos))
            return false;
        return y == ((Pos) obj).y && x == ((Pos) obj).x;
    }
    @Override
    public int hashCode(){
        // Must the factor be prime?
        return y * 1021 + x;
    }
}
