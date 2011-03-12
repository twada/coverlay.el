require 'rubygems'
require 'spec'
require 'tqcov'


describe TqCov do
  before do
    lines = <<-EOS
init.js,1,2
init.js,2,2
init.js,4,1
init.js,5,1
init.js,8,1
init.js,9,1
init.js,11,1
init.js,12,1
init.js,15,1
init.js,16,1
init.js,17,1
init.js,18,1
init.js,19,0
init.js,20,0
init.js,22,1
init.js,23,1
init.js,25,1
ProJavaScriptDesignPatterns.js,1,2
ProJavaScriptDesignPatterns.js,3,1
ProJavaScriptDesignPatterns.js,4,0
ProJavaScriptDesignPatterns.js,5,0
ProJavaScriptDesignPatterns.js,6,0
ProJavaScriptDesignPatterns.js,9,1
ProJavaScriptDesignPatterns.js,10,43
ProJavaScriptDesignPatterns.js,11,43
ProJavaScriptDesignPatterns.js,12,43
ProJavaScriptDesignPatterns.js,13,43
ProJavaScriptDesignPatterns.js,15,43
ProJavaScriptDesignPatterns.js,16,43
ProJavaScriptDesignPatterns.js,17,0
ProJavaScriptDesignPatterns.js,21,1
ProJavaScriptDesignPatterns.js,22,0
ProJavaScriptDesignPatterns.js,23,0
ProJavaScriptDesignPatterns.js,24,0
ProJavaScriptDesignPatterns.js,27,0
ProJavaScriptDesignPatterns.js,28,0
ProJavaScriptDesignPatterns.js,29,0
ProJavaScriptDesignPatterns.js,35,1
ProJavaScriptDesignPatterns.js,36,97
ProJavaScriptDesignPatterns.js,37,0
ProJavaScriptDesignPatterns.js,38,0
ProJavaScriptDesignPatterns.js,41,97
ProJavaScriptDesignPatterns.js,42,194
ProJavaScriptDesignPatterns.js,43,194
ProJavaScriptDesignPatterns.js,44,100
ProJavaScriptDesignPatterns.js,53,1
ProJavaScriptDesignPatterns.js,54,3
ProJavaScriptDesignPatterns.js,55,0
ProJavaScriptDesignPatterns.js,58,3
ProJavaScriptDesignPatterns.js,59,3
ProJavaScriptDesignPatterns.js,60,3
ProJavaScriptDesignPatterns.js,61,7
ProJavaScriptDesignPatterns.js,62,0
ProJavaScriptDesignPatterns.js,64,7
ProJavaScriptDesignPatterns.js,68,1
ProJavaScriptDesignPatterns.js,69,499
ProJavaScriptDesignPatterns.js,70,0
ProJavaScriptDesignPatterns.js,73,499
ProJavaScriptDesignPatterns.js,74,499
ProJavaScriptDesignPatterns.js,75,499
ProJavaScriptDesignPatterns.js,76,0
ProJavaScriptDesignPatterns.js,79,499
ProJavaScriptDesignPatterns.js,80,1209
ProJavaScriptDesignPatterns.js,81,1209
ProJavaScriptDesignPatterns.js,82,0
EOS
    io = StringIO.new(lines)
    @cov = TqCov::Repository.new
    @cov.parse(io)
  end

  context "init.js coverage" do
    subject { @cov["init.js"] }
    its(:to_ary) { should == [[19, 20]] }
    its(:sexp) { should == "((19 20))" }
  end

  context "unknown file" do
    subject { @cov["hoge.js"] }
    its(:to_ary) { should == [] }
    its(:sexp) { should == "()" }
  end

  context "ProJavaScriptDesignPatterns coverage" do
    subject { @cov["ProJavaScriptDesignPatterns.js"] }
    its(:to_ary) { should == [[4, 6], [17, 17], [22, 24], [27, 29], [37, 38], [55, 55], [62, 62], [70, 70], [76, 76], [82, 82]] }
    its(:sexp) { should == "((4 6) (17 17) (22 24) (27 29) (37 38) (55 55) (62 62) (70 70) (76 76) (82 82))" }
  end
end
