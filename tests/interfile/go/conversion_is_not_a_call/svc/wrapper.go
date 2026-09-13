package svc

type Wrapper string

func (w Wrapper) Report(q string) {
	// ok: conversion-is-not-a-call
	sink(q)
}

func Run(q string) {
	// ruleid: conversion-is-not-a-call
	sink(q)
}
