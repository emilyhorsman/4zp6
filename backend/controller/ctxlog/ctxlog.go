package ctxlog

import (
	"context"

	"github.com/sirupsen/logrus"
)

// ctxLogKey is an key for context values.
type ctxLogKey struct{}

// ctxLog is a contextual logger.
type ctxLog struct {
	logger *logrus.Entry // logger is logrus root
	fields logrus.Fields // fields are appended fields
}

// key is a context key instance.
var key = &ctxLogKey{}

// WithFields injects the logrus entry into the provided context, returning the
// new context.
func WithFields(ctx context.Context, entry *logrus.Entry) context.Context {
	l := &ctxLog{
		logger: entry,
		fields: logrus.Fields{},
	}
	return context.WithValue(ctx, key, l)
}

// AddFields appends new fields or updates existing fields in context.
func AddFields(ctx context.Context, fields logrus.Fields) {
	// fetch context logger
	l, ok := ctx.Value(key).(*ctxLog)
	if !ok || l == nil {
		// no operation
		return
	}
	// update the fields with provided ones
	for key, val := range fields {
		l.fields[key] = val
	}
}

// Log returns the logrus entry from context. If a logger was not placed into
// context, a new logger entry will be returned.
func Log(ctx context.Context) *logrus.Entry {
	// fetch context logger
	l, ok := ctx.Value(key).(*ctxLog)
	if !ok || l == nil {
		return logrus.NewEntry(logrus.New())
	}
	return l.logger.WithFields(l.fields)
}
