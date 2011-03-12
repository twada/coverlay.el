module TqCov
  class Segment
    def initialize(from, to)
      @from, @to = from, to
    end
    attr_accessor :from, :to
    def to_ary
      [from, to]
    end
    def sexp
      "(#{from} #{to})"
    end
  end

  class StatementCoverage
    def initialize(file_path)
      @file_path = file_path
      @segments = []
    end
    def to_ary
      @segments.map(&:to_ary)
    end
    def sexp
      "(" << @segments.map(&:sexp).join(" ") << ")"
    end
    def last
      @segments.last
    end
    def <<(seg)
      @segments << seg
    end
  end

  class Repository
    def initialize
      @coverages = {}
    end

    def [](file_path)
      if @coverages.include?(file_path)
        @coverages[file_path]
      else
        StatementCoverage.new(file_path)
      end
    end

    def parse(io)
      io.each_line do |line|
        file_path, line_no, count = line.split(',')
        @coverages[file_path] = TqCov::StatementCoverage.new(file_path) unless @coverages.include?(file_path)
        line_no = line_no.to_i
        count = count.to_i
        next unless count == 0
        current_segment = @coverages[file_path].last
        if current_segment and current_segment.to == line_no - 1
          current_segment.to = line_no
        else
          @coverages[file_path] << TqCov::Segment.new(line_no, line_no)
        end
      end
    end
  end

end


if $0 == __FILE__
  covfile = File.expand_path(File.join(File.dirname(__FILE__), "coverage_stats.csv"))
  repo = TqCov::Repository.new
  repo.parse(File.new(covfile))
  file_path = ARGV.shift
  print repo[file_path].sexp
end
