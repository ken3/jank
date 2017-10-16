
# jank

a study for mahjong

### Usage

【ロード】  
  irb> load 'tinyjank.rb'  
  => true  

【判定例 1: 国士無双】  
  irb> p [1,9,11,19,21,29,31,33,35,37,41,43,45,45].candies  
  #<Set: {{:type=>:国士無双,  
    :body=>[[1, 9, 11, 19, 21, 29, 31, 33, 35, 37, 41, 43, 45, 45]]}}>  

【判定例 2: 三連刻 or 三暗刻 or 一盃口】  
  irb> p [1,1,2,2,2,3,3,3,4,4,4,26,27,28].candies  
  #<Set: {  
   {:type=>:面子,  
    :head=>[1, 1],  
    :body=>[[2, 2, 2], [3, 3, 3], [4, 4, 4], [26, 27, 28]]},  
   {:type=>:面子,  
    :head=>[1, 1],  
    :body=>[[2, 3, 4], [2, 3, 4], [2, 3, 4], [26, 27, 28]]},  
   {:type=>:面子,  
    :head=>[4, 4],  
    :body=>[[1, 2, 3], [1, 2, 3], [2, 3, 4], [26, 27, 28]]}}>  

【判定例 3: 七対子 or 二盃口(3パターン)】  
  irb> p [1,1,2,2,3,3,4,4,5,5,6,6,7,7].candies  
  #<Set: {  
   {:type=>:七対子,  
    :body=>[[1, 1], [2, 2], [3, 3], [4, 4], [5, 5], [6, 6], [7, 7]]},  
   {:type=>:面子,  
    :head=>[1, 1],  
    :body=>[[2, 3, 4], [2, 3, 4], [5, 6, 7], [5, 6, 7]]},  
   {:type=>:面子,  
    :head=>[4, 4],  
    :body=>[[1, 2, 3], [1, 2, 3], [5, 6, 7], [5, 6, 7]]},  
   {:type=>:面子,  
    :head=>[7, 7],  
    :body=>[[1, 2, 3], [1, 2, 3], [4, 5, 6], [4, 5, 6]]}}>  

