#!/usr/bin/ruby
# -*- mode=ruby; coding:utf-8 -*-

# 機能: 麻雀の役判定と点数計算
# 作成: 2017-10-04  ken3@nurs.or.jp
# 更新: 2017-10-16  ken3@nurs.or.jp

require 'set'

# 表示用データ
Image = [:■, :一, :二, :三, :四, :五, :六, :七, :八, :九,  # 萬子
         :■, :①, :②, :③, :④, :⑤, :⑥, :⑦, :⑧, :⑨,  # 筒子
         :■, :Ⅰ, :Ⅱ, :Ⅲ, :Ⅳ, :Ⅴ, :Ⅵ, :Ⅶ, :Ⅷ, :Ⅸ,  # 索子
         :■, :東, :■, :南, :■, :西, :■, :北, :■, :■,  # 風牌
         :■, :白, :■, :発, :■, :中, :■]                 # 三元牌

# 判定用データ
Type  = {
    :萬子牌 => [1..9],
    :筒子牌 => [11..19],
    :索子牌 => [21..29],
    :老頭牌 => [1,9,11,19,21,29],
    :四風牌 => [31,33,35,37],
    :三元牌 => [41,43,45],
    :幺九牌 => [1,9,11,19,21,29,31,33,35,37,41,43,45],
    :緑色牌 => [22,23,24,26,28,43]
}

# Arrayクラスの拡張
module Mahjong
public
    # 成立する組み合せの集合を返す
    def candies
        acc = Set.new << self.七対子? << self.国士無双?
        acc += self.雀頭.inject(Set.new) {|a,head|
            # 1雀頭+N面子のすべての組み合わせを探す
            a += sub(head).breakdown.inject(Set.new) {|r,m|
                r<<{:type => :面子, :head => head, :body =>m.sort}
            }
        }
        acc.delete(nil)
    end

    # 画面表示
    def show
        each {|x|
            case x
            when NilClass
                # Do Nothing
            when Array
                print "[" + x.map {|y| Image[y]}.join(" ") + "]"
            else
                print "#{Image[x]} "
            end
        }
        puts
    end

protected
    # 面子の組合せを作成する
    def make_combinations(head, rest)
        case rest.size
        when self.size
            []
        when 0
            [[self]]
        else
            rest.breakdown.inject([]) {|r,x| r<<x.unshift(head)}
        end
    end

    # N面子に分解する
    def breakdown
        f = lambda {|r,x| r += make_combinations(x, sub(x))}
        acc = Set.new
        acc += 刻子.inject(Set.new, &f) # 刻子優先の組合せ
        acc += 順子.inject(Set.new, &f) # 順子優先の組合せ
    end

    # 差集合を返す
    # 標準の - は重複要素をすべて削除するが、subは重複要素を削除しない。
    def sub(set)
        set.each_with_object(self.dup) {|x,r| t = r.index(x) and r.delete_at(t)}
    end

    # ヒストグラムを返す
    def histogram
        (0..45).map {|i| count(i)}
    end

    # 雀頭候補を返す
    def 雀頭
        histogram.zip(0..45).each_with_object([]) {|(x,i),r| r<<[i,i] if x >= 2}
    end

    # 刻子候補を返す
    def 刻子
        histogram.zip(0..45).each_with_object([]) {|(x,i),r| r<<[i,i,i] if x >= 3}
    end

    # 順子候補を返す
    def 順子
        h = histogram
        (0..27).map {|i| h[i]*h[i+1]*h[i+2] > 0 ? [i,i+1,i+2] : nil}.compact
    end

    # 七対子判定
    def 七対子?
        count = histogram.inject(0) {|r,x| x == 2 ? r+1 : r}
        {:type => :七対子, :body => group_by{|x| x}.values.sort} if count == 7
    end

    # 国士無双判定
    def 国士無双?
        if self.count == 14 and t = sub(Type[:幺九牌]) and t.count == 1
            {:type => :国士無双, :body => [self.sort]} if t.sub(Type[:幺九牌]).empty?
        end
    end
end
class Array
    include Mahjong
end

# Setクラスの拡張
class Set
    # 画面表示
    def show
        each {|h| ([h[:head]] + h[:body]).show}
    end
end

# テスト
#[1,9,11,19,21,29,31,33,35,37,41,43,45,45].tap {|x| # 国士無双
#    p x.candies
#    x.candies.show
#}
#[1,1,2,2,3,3,4,4,5,5,6,6,7,7].tap {|x|             # 七対子 or 二盃口(3パターン)
#    p x.candies
#    x.candies.show
#}
#[1,1,2,2,2,3,3,3,4,4,4,26,27,28].tap {|x|          # 三連刻 or 三暗刻 or 一盃口
#    p x.candies
#    x.candies.show
#}

