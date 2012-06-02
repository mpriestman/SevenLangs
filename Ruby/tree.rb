class Tree
  attr_accessor :children, :node_name
  
  def initialize(nodes)
    @node_name = "root"
    @children = []
    
    if nodes.size == 1
      @node_name = nodes.keys()[0]
      nodes[@node_name].each_pair { |key, value| @children.push(Tree.new({ key => value })) }
    else
      nodes.each_pair { |key, value| @children.push(Tree.new({ key => value })) }
    end
  end
  
  def visit_all(&block)
    visit &block
    children.each { |c| c.visit_all &block }
  end
  
  def visit(&block)
    block.call self
  end
end

tree = {'grandpa' => {'dad' => {'child 1' => {}, 'child 2' => {}}, 'uncle' => {'child 3' => {}, 'child 4' => {} } } }

ruby_tree = Tree.new(tree)
   
ruby_tree.visit { |node| puts node.node_name }

ruby_tree.visit_all { |node| puts node.node_name }