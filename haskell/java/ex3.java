package combs;

import java.util.ArrayList;
import java.util.List;

import data.LList;

/**
 * @author mattf
 *
 * Succeeds if *all* of its parsers succeed, in sequence.
 * Fails if any of its parsers fail, in sequence.
 * 
 */
public class All<T, A> extends Parser<T, List<A>> {
	
	private final List<Parser<T, A>> parsers;

	public All(List<Parser<T, A>> parsers) {
		this.parsers = parsers;
	}

	@Override
	public ParseResult<T, List<A>> parse(LList<T> tokens) {
		List<A> results = new ArrayList<A>();
		LList<T> restTokens = tokens;
		for(Parser<T,A> p : this.parsers) {
			ParseResult<T, A> r = p.parse(restTokens);
			if(r.isSuccess()) {
				restTokens = r.getRestTokens();
				results.add(r.getValue());
			} else {
				return ParseResult.failure("'all' failed", restTokens);
			}
		}
		return ParseResult.success(restTokens, results);
	}
}