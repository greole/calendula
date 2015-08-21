import Test.HUnit
import Data.Monoid
import Control.Monad
import MiniMarkdown

testWriteHTMLString :: (String, String) -> Test
testWriteHTMLString (inp, out) =
     TestCase $ assertEqual "" out $ writeHTMLString inp

main :: IO Counts
main = do 
    let testcases = [("**foo**", "<strong>foo</strong>"),
           ("**foo**foo" , "<strong>foo</strong>foo"),
           ("**foo**foo*bar*" , "<strong>foo</strong>foo<em>bar</em>"),
           ("*foo*" , "<em>foo</em>"),
           --("* foo \n" , "<li>foo</li>"),
           ("Stuff" , "Stuff"),
           ("Stuff*foo*" , "Stuff<em>foo</em>"),
           ("Stuff*foo***bar**" , "Stuff<em>foo</em><strong>bar</strong>"),
           ("#Head\nStuff*foo***bar**" , "<h1>Head</h1>Stuff<em>foo</em><strong>bar</strong>"),
           ("#Head\n**foo***bar*" , "<h1>Head</h1><strong>foo</strong><em>bar</em>"),
           ("# Head\n" , "<h1>Head</h1>"),
           ("## Head\n" , "<h2>Head</h2>"),
           ("### Head\n" , "<h3>Head</h3>"),
           ("#### SUBTOPIC1\n" , "<h4>SUBTOPIC1</h4>"),
           ("foo #### SUBTOPIC1\n" , "foo <h4>SUBTOPIC1</h4>"),
           ("# Head\n## Head\n" , "<h1>Head</h1><h2>Head</h2>"),
           ("# Head\n## Head\n### Head\n" , "<h1>Head</h1><h2>Head</h2><h3>Head</h3>"),
           ("[foo](www.foo.com)" , "<a href=\"www.foo.com\">foo</a>"),
           ("`foo`", "<code>foo</code>"),
           ("~~~\nfoo\n~~~\n", "<p><code>foo\n</code></p>")]
    let tests = map testWriteHTMLString testcases 
    runTestTT $ TestList tests
