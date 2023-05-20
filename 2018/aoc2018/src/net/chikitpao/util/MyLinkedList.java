// MyLinkedList.java
// AoC 2018
// Author: Chi-Kit Pao
//

package net.chikitpao.util;

import java.util.NoSuchElementException;

public class MyLinkedList<T> {
    private class MyLinkedListItem<T>
    {
        private MyLinkedListItem previous = null;
        private MyLinkedListItem next = null;
        private T data = null;
    }

    public class Iterator<T>
    {
        private MyLinkedList<T> linkedList;
        private MyLinkedListItem<T> previous;
        private MyLinkedListItem<T> next;

        private Iterator(MyLinkedList<T> linkedList, MyLinkedListItem<T> previous, MyLinkedListItem<T> next){
            this.linkedList = linkedList;
            this.previous = previous;
            this.next = next;
        }
        public boolean hasPrevious(){
            return previous != linkedList.begin;
        }
        public void previous(){
            if(previous == linkedList.begin)
                throw new NoSuchElementException();
            previous = previous.previous;
            next = previous.next;
        }
        public boolean hasNext(){
            return next != linkedList.end;
        }
        public void next(){
            if(next == linkedList.end)
                throw new NoSuchElementException();
            next = next.next;
            previous = next.previous;
        }
        public void add(T value){
            MyLinkedListItem<T> item = new MyLinkedListItem<>();
            previous.next = item;
            next.previous = item;
            item.previous = previous;
            item.next = next;
            next = item;
            item.data = value;
        }
        public T get(){
            if(next == linkedList.end)
                throw new NoSuchElementException();
            return next.data;
        }
        public void set(T value){
            if(next == linkedList.end)
                throw new NoSuchElementException();
            next.data = value;
        }
        public T remove(){
            if(next == linkedList.end)
                throw new NoSuchElementException();
            MyLinkedListItem<T> item = next;

            next.next.previous = item.previous;
            previous.next = item.next;
            next = item.next;

            item.previous = null;
            item.next = null;
            return item.data;
        }

        public void move(int offset, boolean isCircular) {
            if(offset == 0)
                return;

            if(offset > 0) {
                for(int i = 0; i < offset; ++i) {
                    if(isCircular && next == linkedList.end) {
                        previous = linkedList.begin;
                        next = linkedList.begin.next;
                    }
                    if(hasNext()){
                        next();
                        continue;
                    }
                    if (!isCircular)
                        throw new NoSuchElementException();
                    else {
                        previous = linkedList.begin;
                        next = linkedList.begin.next;
                    }
                }
                return;
            }

            // offset < 0
            for(int i = offset; i < 0; ++i) {
                if(hasPrevious()){
                    previous();
                    continue;
                }

                if(!isCircular)
                    throw new NoSuchElementException();
                else {
                    if(linkedList.end.previous == linkedList.begin){
                        next = linkedList.end;
                        previous = linkedList.begin;
                    } else {
                        next = linkedList.end.previous;
                        previous = next.previous;
                    }
                }
            }
        }
    }

    private MyLinkedListItem begin;
    private MyLinkedListItem end;

    public MyLinkedList(){
        begin = new MyLinkedListItem<T>();
        end = new MyLinkedListItem<T>();
        begin.next = end;
        end.previous = begin;
    }

    public Iterator<T> begin(){
        return new Iterator<>(this, begin, begin.next);
    }
    public Iterator<T> end(){
        return new Iterator<>(this, end.previous, end);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        Iterator<T> iterator = begin();
        while(iterator.hasNext()){
            sb.append(iterator.get().toString());
            iterator.next();
            if(iterator.hasNext())
                sb.append(", ");
        }
        sb.append("]");
        return sb.toString();
    }
}
